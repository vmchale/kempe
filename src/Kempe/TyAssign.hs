{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Constraint-based typing from the presentation in Pierce's book.
module Kempe.TyAssign ( TypeM
                      , runTypeM
                      , checkModule
                      , assignModule
                      ) where

import           Control.Composition        (thread, (.$))
import           Control.Monad              (foldM, replicateM, unless, when, zipWithM_)
import           Control.Monad.Except       (throwError)
import           Control.Monad.State.Strict (StateT, get, gets, modify, put, runStateT)
import           Data.Bifunctor             (bimap, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Tuple.Extra           (fst3)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens', over)
import           Lens.Micro.Mtl             (modifying, (.=))
import           Prettyprinter              (Doc, Pretty (pretty), hardline, indent, vsep, (<+>))
import           Prettyprinter.Ext

type TyEnv a = IM.IntMap (StackType a)

data TyState a = TyState { maxU             :: Int -- ^ For renamer
                         , tyEnv            :: TyEnv a
                         , kindEnv          :: IM.IntMap Kind
                         , renames          :: IM.IntMap Int
                         , constructorTypes :: IM.IntMap (StackType a)
                         , constraints      :: S.Set (KempeTy a, KempeTy a) -- Just need equality between simple types? (do have tyapp but yeah)
                         }

(<#*>) :: Doc a -> Doc a -> Doc a
(<#*>) x y = x <> hardline <> indent 2 y

instance Pretty (TyState a) where
    pretty (TyState _ te _ r _ cs) =
        "type environment:" <#> vsep (prettyBound <$> IM.toList te)
            <#> "renames:" <#*> prettyDumpBinds r
            <#> "constraints:" <#> prettyConstraints cs

prettyConstraints :: S.Set (KempeTy a, KempeTy a) -> Doc ann
prettyConstraints cs = vsep (prettyEq <$> S.toList cs)

prettyBound :: (Int, StackType a) -> Doc b
prettyBound (i, e) = pretty i <+> "←" <#*> pretty e

prettyEq :: (KempeTy a, KempeTy a) -> Doc ann
prettyEq (ty, ty') = pretty ty <+> "≡" <+> pretty ty'

prettyDumpBinds :: Pretty b => IM.IntMap b -> Doc a
prettyDumpBinds b = vsep (prettyBind <$> IM.toList b)

prettyBind :: Pretty b => (Int, b) -> Doc a
prettyBind (i, j) = pretty i <+> "→" <+> pretty j

emptyStackType :: StackType a
emptyStackType = StackType mempty [] []

maxULens :: Lens' (TyState a) Int
maxULens f s = fmap (\x -> s { maxU = x }) (f (maxU s))

constructorTypesLens :: Lens' (TyState a) (IM.IntMap (StackType a))
constructorTypesLens f s = fmap (\x -> s { constructorTypes = x }) (f (constructorTypes s))

tyEnvLens :: Lens' (TyState a) (TyEnv a)
tyEnvLens f s = fmap (\x -> s { tyEnv = x }) (f (tyEnv s))

kindEnvLens :: Lens' (TyState a) (IM.IntMap Kind)
kindEnvLens f s = fmap (\x -> s { kindEnv = x }) (f (kindEnv s))

renamesLens :: Lens' (TyState a) (IM.IntMap Int)
renamesLens f s = fmap (\x -> s { renames = x }) (f (renames s))

constraintsLens :: Lens' (TyState a) (S.Set (KempeTy a, KempeTy a))
constraintsLens f s = fmap (\x -> s { constraints = x }) (f (constraints s))

dummyName :: T.Text -> TypeM () (Name ())
dummyName n = do
    pSt <- gets maxU
    Name n (Unique $ pSt + 1) ()
        <$ modifying maxULens (+1)

data Kind = Star
          | TyCons Kind Kind
          deriving (Eq)

type TypeM a = StateT (TyState a) (Either (Error a))

onType :: (Int, KempeTy a) -> KempeTy a -> KempeTy a
onType _ ty'@TyBuiltin{} = ty'
onType _ ty'@TyNamed{}   = ty'
onType (k, ty) ty'@(TyVar _ (Name _ (Unique i) _)) | i == k = ty
                                                   | otherwise = ty'
onType (k, ty) (TyApp l ty' ty'') = TyApp l (onType (k, ty) ty') (onType (k, ty) ty'') -- I think this is right

renameForward :: (Int, KempeTy a) -> [(KempeTy a, KempeTy a)] -> [(KempeTy a, KempeTy a)]
renameForward _ []                      = []
renameForward (k, ty) ((ty', ty''):tys) = (onType (k, ty) ty', onType (k, ty) ty'') : renameForward (k, ty) tys

unify :: [(KempeTy a, KempeTy a)] -> Either (Error ()) (IM.IntMap (KempeTy ()))
unify []                                                             = Right mempty
unify ((ty@(TyBuiltin _ b0), ty'@(TyBuiltin _ b1)):tys) | b0 == b1   = unify tys
                                                        | otherwise  = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@(TyNamed _ n0), ty'@(TyNamed _ n1)):tys) | n0 == n1       = unify tys
                                                    | otherwise      = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@(TyNamed _ _), TyVar  _ (Name _ (Unique k) _)):tys)       = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys) -- is this O(n^2) or something bad?
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyNamed _ _)):tys)        = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys) -- FIXME: is renameForward enough?
unify ((ty@(TyBuiltin _ _), TyVar  _ (Name _ (Unique k) _)):tys)     = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyBuiltin _ _)):tys)      = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyVar _ _)):tys)          = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((ty@TyBuiltin{}, ty'@TyNamed{}):_)                            = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyNamed{}, ty'@TyBuiltin{}):_)                            = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyBuiltin{}, ty'@TyApp{}):_)                              = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyNamed{}, ty'@TyApp{}):_)                                = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyApp{}, ty'@TyBuiltin{}):_)                              = Left (UnificationFailed () (void ty) (void ty'))
unify ((TyVar _ (Name _ (Unique k) _), ty@TyApp{}):tys)              = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((ty@TyApp{}, TyVar  _ (Name _ (Unique k) _)):tys)             = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyApp _ ty ty', TyApp _ ty'' ty'''):tys)                     = unify ((ty, ty'') : (ty', ty''') : tys) -- TODO: I think this is right?
unify ((ty@TyApp{}, ty'@TyNamed{}):_)                                = Left (UnificationFailed () (void ty) (void ty'))

unifyM :: S.Set (KempeTy a, KempeTy a) -> TypeM () (IM.IntMap (KempeTy ()))
unifyM s =
    case {-# SCC "unify" #-} unify (S.toList s) of
        Right x  -> pure x
        Left err -> throwError err

-- TODO: take constructor types as an argument?..
runTypeM :: Int -- ^ For renamer
         -> TypeM a x -> Either (Error a) (x, Int)
runTypeM maxInt = fmap (second maxU) .
    flip runStateT (TyState maxInt mempty mempty mempty mempty S.empty)

typeOfBuiltin :: BuiltinFn -> TypeM () (StackType ())
typeOfBuiltin Drop = do
    aN <- dummyName "a"
    pure $ StackType (S.singleton aN) [TyVar () aN] []
typeOfBuiltin Swap = do
    aN <- dummyName "a"
    bN <- dummyName "b"
    pure $ StackType (S.fromList [aN, bN]) [TyVar () aN, TyVar () bN] [TyVar () bN, TyVar () aN]
typeOfBuiltin Dup = do
    aN <- dummyName "a"
    pure $ StackType (S.singleton aN) [TyVar () aN] [TyVar () aN, TyVar () aN]
typeOfBuiltin IntEq      = pure intRel
typeOfBuiltin IntLeq     = pure intRel
typeOfBuiltin IntLt      = pure intRel
typeOfBuiltin IntMod     = pure intBinOp
typeOfBuiltin IntDiv     = pure intBinOp
typeOfBuiltin IntPlus    = pure intBinOp
typeOfBuiltin IntTimes   = pure intBinOp
typeOfBuiltin IntMinus   = pure intBinOp
typeOfBuiltin IntShiftR  = pure intShift
typeOfBuiltin IntShiftL  = pure intBinOp
typeOfBuiltin IntXor     = pure intBinOp
typeOfBuiltin WordXor    = pure wordBinOp
typeOfBuiltin WordPlus   = pure wordBinOp
typeOfBuiltin WordTimes  = pure wordBinOp
typeOfBuiltin WordShiftR = pure wordShift
typeOfBuiltin WordShiftL = pure wordShift
typeOfBuiltin IntGeq     = pure intBinOp
typeOfBuiltin IntNeq     = pure intBinOp
typeOfBuiltin IntGt      = pure intBinOp
typeOfBuiltin WordMinus  = pure wordBinOp
typeOfBuiltin WordDiv    = pure wordBinOp
typeOfBuiltin WordMod    = pure wordBinOp
typeOfBuiltin And        = pure boolOp
typeOfBuiltin Or         = pure boolOp
typeOfBuiltin Xor        = pure boolOp
typeOfBuiltin IntNeg     = pure $ StackType S.empty [TyBuiltin () TyInt] [TyBuiltin () TyInt]
typeOfBuiltin Popcount   = pure $ StackType S.empty [TyBuiltin () TyWord] [TyBuiltin () TyInt]

boolOp :: StackType ()
boolOp = StackType S.empty [TyBuiltin () TyBool, TyBuiltin () TyBool] [TyBuiltin () TyBool]

intRel :: StackType ()
intRel = StackType S.empty [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyBool]

intBinOp :: StackType ()
intBinOp = StackType S.empty [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyInt]

intShift :: StackType ()
intShift = StackType S.empty [TyBuiltin () TyInt, TyBuiltin () TyInt8] [TyBuiltin () TyInt]

wordBinOp :: StackType ()
wordBinOp = StackType S.empty [TyBuiltin () TyWord, TyBuiltin () TyWord] [TyBuiltin () TyWord]

wordShift :: StackType ()
wordShift = StackType S.empty [TyBuiltin () TyWord, TyBuiltin () TyInt8] [TyBuiltin () TyWord]

tyLookup :: Name a -> TypeM a (StackType a)
tyLookup n@(Name _ (Unique i) l) = do
    st <- gets tyEnv
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError $ PoorScope l n

consLookup :: TyName a -> TypeM a (StackType a)
consLookup tn@(Name _ (Unique i) l) = do
    st <- gets constructorTypes
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError $ PoorScope l tn

-- expandType 1
dipify :: StackType () -> TypeM () (StackType ())
dipify (StackType fvrs is os) = do
    n <- dummyName "a"
    pure $ StackType (S.insert n fvrs) (is ++ [TyVar () n]) (os ++ [TyVar () n])

tyLeaf :: (Pattern b a, [Atom b a]) -> TypeM () (StackType ())
tyLeaf (p, as) = do
    tyP <- tyPattern p
    tyA <- tyAtoms as
    catTypes tyP tyA

assignCase :: (Pattern b a, [Atom b a]) -> TypeM () (StackType (), Pattern (StackType ()) (StackType ()), [Atom (StackType ()) (StackType ())])
assignCase (p, as) = do
    (tyP, p') <- assignPattern p
    (as', tyA) <- assignAtoms as
    (,,) <$> catTypes tyP tyA <*> pure p' <*> pure as'

tyAtom :: Atom b a -> TypeM () (StackType ())
tyAtom (AtBuiltin _ b) = typeOfBuiltin b
tyAtom BoolLit{}       = pure $ StackType mempty [] [TyBuiltin () TyBool]
tyAtom IntLit{}        = pure $ StackType mempty [] [TyBuiltin () TyInt]
tyAtom Int8Lit{}       = pure $ StackType mempty [] [TyBuiltin () TyInt8 ]
tyAtom WordLit{}       = pure $ StackType mempty [] [TyBuiltin () TyWord]
tyAtom (AtName _ n)    = renameStack =<< tyLookup (void n)
tyAtom (Dip _ as)      = dipify =<< tyAtoms as
tyAtom (AtCons _ tn)   = renameStack =<< consLookup (void tn)
tyAtom (If _ as as')   = do
    tys <- tyAtoms as
    tys' <- tyAtoms as'
    (StackType vars ins out) <- mergeStackTypes tys tys'
    pure $ StackType vars (ins ++ [TyBuiltin () TyBool]) out
tyAtom (Case _ ls) = do
    tyLs <- traverse tyLeaf ls
    -- TODO: one-pass fold?
    mergeMany tyLs

assignAtom :: Atom b a -> TypeM () (StackType (), Atom (StackType ()) (StackType ()))
assignAtom (AtBuiltin _ b) = do { ty <- typeOfBuiltin b ; pure (ty, AtBuiltin ty b) }
assignAtom (BoolLit _ b)   =
    let sTy = StackType mempty [] [TyBuiltin () TyBool]
        in pure (sTy, BoolLit sTy b)
assignAtom (IntLit _ i)    =
    let sTy = StackType mempty [] [TyBuiltin () TyInt]
        in pure (sTy, IntLit sTy i)
assignAtom (Int8Lit _ i)    =
    let sTy = StackType mempty [] [TyBuiltin () TyInt8]
        in pure (sTy, Int8Lit sTy i)
assignAtom (WordLit _ u)    =
    let sTy = StackType mempty [] [TyBuiltin () TyWord]
        in pure (sTy, WordLit sTy u)
assignAtom (AtName _ n) = do
    sTy <- renameStack =<< tyLookup (void n)
    pure (sTy, AtName sTy (n $> sTy))
assignAtom (AtCons _ tn) = do
    sTy <- renameStack =<< consLookup (void tn)
    pure (sTy, AtCons sTy (tn $> sTy))
assignAtom (Dip _ as)    = do { (as', ty) <- assignAtoms as ; tyDipped <- dipify ty ; pure (tyDipped, Dip tyDipped as') }
assignAtom (If _ as0 as1) = do
    (as0', tys) <- assignAtoms as0
    (as1', tys') <- assignAtoms as1
    (StackType vars ins out) <- mergeStackTypes tys tys'
    let resType = StackType vars (ins ++ [TyBuiltin () TyBool]) out
    pure (resType, If resType as0' as1')
assignAtom (Case _ ls) = do
    lRes <- traverse assignCase ls
    resType <- mergeMany (fst3 <$> lRes)
    let newLeaves = fmap dropFst lRes
    pure (resType, Case resType newLeaves)
    where dropFst (_, y, z) = (y, z)

assignAtoms :: [Atom b a] -> TypeM () ([Atom (StackType ()) (StackType ())], StackType ())
assignAtoms = foldM
    (\seed a -> do { (ty, r) <- assignAtom a ; (fst seed ++ [r] ,) <$> catTypes (snd seed) ty })
    ([], emptyStackType)

tyAtoms :: [Atom b a] -> TypeM () (StackType ())
tyAtoms = foldM
    (\seed a -> do { tys' <- tyAtom a ; catTypes seed tys' })
    emptyStackType

-- from size,
mkHKT :: Int -> Kind
mkHKT 0 = Star
mkHKT i = TyCons (mkHKT $ i - 1) Star

tyInsertLeaf :: Name b -- ^ type being declared
             -> S.Set (Name b) -> (TyName a, [KempeTy b]) -> TypeM () ()
tyInsertLeaf n@(Name _ (Unique k) _) vars (Name _ (Unique i) _, ins) | S.null vars =
    modifying constructorTypesLens (IM.insert i (voidStackType $ StackType vars ins [TyNamed undefined n])) *>
    modifying kindEnvLens (IM.insert k Star)
                                               | otherwise =
    modifying constructorTypesLens (IM.insert i (voidStackType $ StackType vars ins [app (TyNamed undefined n) (S.toList vars)])) *>
    modifying kindEnvLens (IM.insert k (mkHKT $ S.size vars))

assignTyLeaf :: Name b
             -> S.Set (Name b)
             -> (TyName a, [KempeTy b])
             -> TypeM () (TyName (StackType ()), [KempeTy ()])
assignTyLeaf n@(Name _ (Unique k) _) vars (tn@(Name _ (Unique i) _), ins) | S.null vars =
    let ty = voidStackType $ StackType vars ins [TyNamed undefined n] in
    modifying constructorTypesLens (IM.insert i ty) *>
    modifying kindEnvLens (IM.insert k Star) $>
    (tn $> ty, fmap void ins)
                                               | otherwise =
    let ty = voidStackType $ StackType vars ins [app (TyNamed undefined n) (S.toList vars)] in
    modifying constructorTypesLens (IM.insert i ty) *>
    modifying kindEnvLens (IM.insert k (mkHKT $ S.size vars)) $>
    (tn $> ty, fmap void ins)

app :: KempeTy a -> [Name a] -> KempeTy a
app = foldr (\n ty -> TyApp undefined ty (TyVar undefined n))

kindLookup :: TyName a -> TypeM a Kind
kindLookup n@(Name _ (Unique i) l) = do
    st <- gets kindEnv
    case IM.lookup i st of
        Just k  -> pure k
        Nothing -> throwError $ PoorScope l n

kindOf :: KempeTy a -> TypeM a Kind
kindOf TyBuiltin{}        = pure Star
kindOf (TyNamed _ tn)     = kindLookup tn
kindOf TyVar{}            = pure Star
kindOf tyErr@(TyApp l ty ty') = do
    k <- kindOf ty
    k' <- kindOf ty'
    case k of
        TyCons k'' k''' -> unless (k' == k''') (throwError (IllKinded l tyErr)) $> k''
        _               -> throwError (IllKinded l tyErr)

assignDecl :: KempeDecl a c b -> TypeM () (KempeDecl () (StackType ()) (StackType ()))
assignDecl (TyDecl _ tn ns ls) = TyDecl () (void tn) (void <$> ns) <$> traverse (assignTyLeaf tn (S.fromList ns)) ls
assignDecl (FunDecl _ n ins os a) = do
    traverse_ kindOf (void <$> ins ++ os)
    sig <- renameStack $ voidStackType $ StackType (freeVars (ins ++ os)) ins os
    (as, inferred) <- assignAtoms a
    reconcile <- mergeStackTypes sig inferred
    -- assign comes after tyInsert
    pure $ FunDecl reconcile (n $> reconcile) (void <$> ins) (void <$> os) as
assignDecl (ExtFnDecl _ n ins os cn) = do
    traverse_ kindOf (void <$> ins ++ os)
    unless (length os <= 1) $
        throwError $ InvalidCImport () (void n)
    let sig = voidStackType $ StackType S.empty ins os
    -- assign always comes after tyInsert
    pure $ ExtFnDecl sig (n $> sig) (void <$> ins) (void <$> os) cn
assignDecl (Export _ abi n) = do
    ty@(StackType _ _ os) <- tyLookup (void n)
    unless (abi == Kabi || length os <= 1) $
        throwError $ InvalidCExport () (void n)
    Export ty abi <$> assignName n

-- don't need to rename cuz it's only for exports (in theory)
assignName :: Name a -> TypeM () (Name (StackType ()))
assignName n = do { ty <- tyLookup (void n) ; pure (n $> ty) }

tyHeader :: KempeDecl a c b -> TypeM () ()
tyHeader Export{} = pure ()
tyHeader (FunDecl _ (Name _ (Unique i) _) ins out _) = do
    let sig = voidStackType $ StackType (freeVars (ins ++ out)) ins out
    modifying tyEnvLens (IM.insert i sig)
tyHeader (ExtFnDecl _ n@(Name _ (Unique i) _) ins os _) = do
    unless (length os <= 1) $
        throwError $ InvalidCImport () (void n)
    unless (null $ freeVars (ins ++ os)) $
        throwError $ TyVarExt () (void n)
    let sig = voidStackType $ StackType S.empty ins os -- no free variables allowed in c functions
    modifying tyEnvLens (IM.insert i sig)
tyHeader TyDecl{} = pure ()

lessGeneral :: StackType a -> StackType a -> Bool
lessGeneral (StackType _ is os) (StackType _ is' os') = lessGenerals (is ++ os) (is' ++ os')
    where lessGeneralAtom :: KempeTy a -> KempeTy a -> Bool
          lessGeneralAtom TyBuiltin{} TyVar{}                   = True
          lessGeneralAtom TyApp{} TyVar{}                       = True
          lessGeneralAtom (TyApp _ ty ty') (TyApp _ ty'' ty''') = lessGeneralAtom ty ty'' || lessGeneralAtom ty' ty''' -- lazy pattern match?
          lessGeneralAtom _ _                                   = False
          lessGenerals :: [KempeTy a] -> [KempeTy a] -> Bool
          lessGenerals [] []               = False
          lessGenerals (ty:tys) (ty':tys') = lessGeneralAtom ty ty' || lessGenerals tys tys'
          lessGenerals _ []                = False -- shouldn't happen; will be caught later
          lessGenerals [] _                = False

tyInsert :: KempeDecl a c b -> TypeM () ()
tyInsert (TyDecl _ tn ns ls) = traverse_ (tyInsertLeaf tn (S.fromList ns)) ls
tyInsert (FunDecl _ _ ins out as) = do
    traverse_ kindOf (void <$> ins ++ out) -- FIXME: this gives sketchy results?
    sig <- renameStack $ voidStackType $ StackType (freeVars (ins ++ out)) ins out
    inferred <- tyAtoms as
    _ <- mergeStackTypes sig inferred -- FIXME: need to verify the merged type is as general as the signature?
    when (inferred `lessGeneral` sig) $
        throwError $ LessGeneral () sig inferred
tyInsert ExtFnDecl{} = pure () -- TODO: kind-check
tyInsert Export{} = pure ()

tyModule :: Module a c b -> TypeM () ()
tyModule m = traverse_ tyHeader m *> traverse_ tyInsert m

checkModule :: Module a c b -> TypeM () ()
checkModule m = tyModule m <* (unifyM =<< gets constraints)

assignModule :: Module a c b -> TypeM () (Module () (StackType ()) (StackType ()))
assignModule m = {-# SCC "assignModule" #-} do
    traverse_ tyHeader m
    m' <- traverse assignDecl m
    backNames <- unifyM =<< gets constraints
    pure (fmap (bimap .$ substConstraintsStack backNames) m')

-- Make sure you don't have cycles in the renames map!
replaceUnique :: Unique -> TypeM a Unique
replaceUnique u@(Unique i) = do
    rSt <- gets renames
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> replaceUnique (Unique j)

renameIn :: KempeTy a -> TypeM a (KempeTy a)
renameIn b@TyBuiltin{}    = pure b
renameIn n@TyNamed{}      = pure n
renameIn (TyApp l ty ty') = TyApp l <$> renameIn ty <*> renameIn ty'
renameIn (TyVar l (Name t u l')) = do
    u' <- replaceUnique u
    pure $ TyVar l (Name t u' l')

-- has to use the max-iest maximum so we can't use withState
withTyState :: (TyState a -> TyState a) -> TypeM a x -> TypeM a x
withTyState modSt act = do
    preSt <- get
    modify modSt
    res <- act
    postMax <- gets maxU
    put preSt
    maxULens .= postMax
    pure res

withName :: Name a -> TypeM a (Name a, TyState a -> TyState a)
withName (Name t (Unique i) l) = do
    m <- gets maxU
    let newUniq = m+1
    maxULens .= newUniq
    pure (Name t (Unique newUniq) l, over renamesLens (IM.insert i (m+1)))

-- freshen the names in a stack so there aren't overlaps in quanitified variables
renameStack :: StackType a -> TypeM a (StackType a)
renameStack (StackType qs ins outs) = do
    newQs <- traverse withName (S.toList qs)
    let (newNames, localRenames) = unzip newQs
        newBinds = thread localRenames
    withTyState newBinds $
        StackType (S.fromList newNames) <$> traverse renameIn ins <*> traverse renameIn outs

mergeStackTypes :: StackType () -> StackType () -> TypeM () (StackType ())
mergeStackTypes st0@(StackType _ i0 o0) st1@(StackType _ i1 o1) = do
    let li0 = length i0
        li1 = length i1
        toExpand = max (abs (li0 - li1)) (abs (length o0 - length o1))

    (StackType q ins os) <- (if li0 < li1 then expandType toExpand else pure) st0
    (StackType q' ins' os') <- (if li1 < li0 then expandType toExpand else pure) st1

    when ((length ins /= length ins') || (length os /= length os')) $
        throwError $ MismatchedLengths () st0 st1

    zipWithM_ pushConstraint ins ins'
    zipWithM_ pushConstraint os os'

    pure $ StackType (q <> q') ins os

tyPattern :: Pattern b a -> TypeM () (StackType ())
tyPattern PatternWildcard{} = do
    aN <- dummyName "a"
    pure $ StackType (S.singleton aN) [TyVar () aN] []
tyPattern PatternInt{} = pure $ StackType S.empty [TyBuiltin () TyInt] []
tyPattern PatternBool{} = pure $ StackType S.empty [TyBuiltin () TyBool] []
tyPattern (PatternCons _ tn) = renameStack =<< (flipStackType <$> consLookup (void tn))

assignPattern :: Pattern b a -> TypeM () (StackType (), Pattern (StackType ()) (StackType ()))
assignPattern (PatternInt _ i) =
    let sTy = StackType S.empty [TyBuiltin () TyInt] []
        in pure (sTy, PatternInt sTy i)
assignPattern (PatternBool _ i) =
    let sTy = StackType S.empty [TyBuiltin () TyBool] []
        in pure (sTy, PatternBool sTy i)
assignPattern (PatternCons _ tn) = do { ty <- renameStack =<< (flipStackType <$> consLookup (void tn)) ; pure (ty, PatternCons ty (tn $> ty)) }
assignPattern PatternWildcard{} = do
    aN <- dummyName "a"
    let resType = StackType (S.singleton aN) [TyVar () aN] []
    pure (resType, PatternWildcard resType)

mergeMany :: NonEmpty (StackType ()) -> TypeM () (StackType ())
mergeMany (t :| ts) = foldM mergeStackTypes t ts

-- assumes they have been renamed...
pushConstraint :: Ord a => KempeTy a -> KempeTy a -> TypeM a ()
pushConstraint ty ty' =
    modifying constraintsLens (S.insert (ty, ty'))

expandType :: Int -> StackType () -> TypeM () (StackType ())
expandType n (StackType q i o) = do
    newVars <- replicateM n (dummyName "a")
    let newTy = TyVar () <$> newVars
    pure $ StackType (q <> S.fromList newVars) (newTy ++ i) (newTy ++ o)

substConstraints :: IM.IntMap (KempeTy a) -> KempeTy a -> KempeTy a
substConstraints _ ty@TyNamed{}                         = ty
substConstraints _ ty@TyBuiltin{}                       = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) =
    case IM.lookup k tys of
        Just ty'@TyVar{} -> substConstraints (IM.delete k tys) ty' -- TODO: this is to prevent cyclic lookups: is it right?
        Just ty'         -> ty'
        Nothing          -> ty
substConstraints tys (TyApp l ty ty')                   =
    TyApp l (substConstraints tys ty) (substConstraints tys ty')

substConstraintsStack :: IM.IntMap (KempeTy a) -> StackType a -> StackType a
substConstraintsStack tys (StackType _ is os) = {-# SCC "substConstraintsStack" #-}
    let is' = substConstraints tys <$> is
        os' = substConstraints tys <$> os
        in StackType (freeVars (is' ++ os')) is' os'

-- do renaming before this
-- | Given @x@ and @y@, return the 'StackType' of @x y@
catTypes :: StackType () -- ^ @x@
         -> StackType () -- ^ @y@
         -> TypeM () (StackType ())
catTypes st0@(StackType _ _ osX) (StackType q1 insY osY) = do
    let lY = length insY
        lDiff = lY - length osX

    -- all of the "ins" of y have to come from x, so we expand x as needed
    (StackType q0 insX osX') <- if lDiff > 0
        then expandType lDiff st0
        else pure st0

    -- zip the last (length insY) of osX' with insY
    zipWithM_ pushConstraint (drop (length osX' - lY) osX') insY -- TODO splitAt

    pure $ StackType (q0 <> q1) insX (take (length osX' - lY) osX' ++ osY)
