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
import           Control.Monad.State.Strict (State, StateT, evalState, get, gets, modify, put, runStateT)
import           Data.Bifunctor             (bimap, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Tuple.Ext             (fst3)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens', over)
import           Lens.Micro.Mtl             (modifying, (.=))
import           Prettyprinter              (Doc, Pretty (pretty), vsep, (<+>))
import           Prettyprinter.Debug
import           Prettyprinter.Ext

type TyEnv a = IM.IntMap (StackType a)

data TyState a = TyState { maxU             :: Int -- ^ For renamer
                         , tyEnv            :: TyEnv a
                         , kindEnv          :: IM.IntMap Kind
                         , renames          :: IM.IntMap Int
                         , constructorTypes :: IM.IntMap (StackType a)
                         , constraints      :: S.Set (KempeTy a, KempeTy a) -- Just need equality between simple types? (do have tyapp but yeah)
                         }

instance Pretty (TyState a) where
    pretty (TyState _ te _ r _ cs) =
        "type environment:" <#> vsep (prettyBound <$> IM.toList te)
            <#> "renames:" <#*> prettyDumpBinds r
            <#> "constraints:" <#> prettyConstraints cs

prettyConstraints :: S.Set (KempeTy a, KempeTy a) -> Doc ann
prettyConstraints cs = vsep (prettyEq <$> S.toList cs)

prettyEq :: (KempeTy a, KempeTy a) -> Doc ann
prettyEq (ty, ty') = pretty ty <+> "≡" <+> pretty ty'

prettyDumpBinds :: Pretty b => IM.IntMap b -> Doc a
prettyDumpBinds b = vsep (prettyBind <$> IM.toList b)

emptyStackType :: StackType a
emptyStackType = StackType [] []

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

type UnifyMap = IM.IntMap (KempeTy ())

inContext :: UnifyMap -> KempeTy () -> KempeTy ()
inContext um ty'@(TyVar _ (Name _ (Unique i) _)) =
    case IM.lookup i um of
        Just ty@TyVar{} -> inContext (IM.delete i um) ty -- prevent cyclic lookups
        -- TODO: does this need a case for TyApp -> inContext?
        Just ty         -> ty
        Nothing         -> ty'
inContext _ ty'@TyBuiltin{} = ty'
inContext _ ty'@TyNamed{} = ty'
inContext um (TyApp l ty ty') = TyApp l (inContext um ty) (inContext um ty')

-- | Perform substitutions before handing off to 'unifyMatch'
unifyPrep :: UnifyMap
          -> [(KempeTy (), KempeTy ())]
          -> Either (Error ()) (IM.IntMap (KempeTy ()))
unifyPrep _ [] = Right mempty
unifyPrep um ((ty, ty'):tys) =
    let ty'' = inContext um ty
        ty''' = inContext um ty'
    in unifyMatch um $ (ty'', ty'''):tys

unifyMatch :: UnifyMap -> [(KempeTy (), KempeTy ())] -> Either (Error ()) (IM.IntMap (KempeTy ()))
unifyMatch _ []                                                              = Right mempty
unifyMatch um ((ty@(TyBuiltin _ b0), ty'@(TyBuiltin _ b1)):tys) | b0 == b1   = unifyPrep um tys
                                                                | otherwise  = Left (UnificationFailed () ty ty')
unifyMatch um ((ty@(TyNamed _ n0), ty'@(TyNamed _ n1)):tys) | n0 == n1       = unifyPrep um tys
                                                            | otherwise      = Left (UnificationFailed () (void ty) (void ty'))
unifyMatch um ((ty@(TyNamed _ _), TyVar  _ (Name _ (Unique k) _)):tys)       = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((TyVar _ (Name _ (Unique k) _), ty@(TyNamed _ _)):tys)        = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((ty@TyBuiltin{}, TyVar  _ (Name _ (Unique k) _)):tys)         = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((TyVar _ (Name _ (Unique k) _), ty@(TyBuiltin _ _)):tys)      = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch _ ((ty@TyBuiltin{}, ty'@TyNamed{}):_)                             = Left (UnificationFailed () ty ty')
unifyMatch _ ((ty@TyNamed{}, ty'@TyBuiltin{}):_)                             = Left (UnificationFailed () ty ty')
unifyMatch _ ((ty@TyBuiltin{}, ty'@TyApp{}):_)                               = Left (UnificationFailed () ty ty')
unifyMatch _ ((ty@TyNamed{}, ty'@TyApp{}):_)                                 = Left (UnificationFailed () ty ty')
unifyMatch _ ((ty@TyApp{}, ty'@TyBuiltin{}):_)                               = Left (UnificationFailed () ty ty')
unifyMatch um ((TyVar _ (Name _ (Unique k) _), ty@TyApp{}):tys)              = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((ty@TyApp{}, TyVar  _ (Name _ (Unique k) _)):tys)             = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((TyApp _ ty ty', TyApp _ ty'' ty'''):tys)                     = unifyPrep um ((ty, ty'') : (ty', ty''') : tys)
unifyMatch _ ((ty@TyApp{}, ty'@TyNamed{}):_)                                 = Left (UnificationFailed () (void ty) (void ty'))
unifyMatch um ((TyVar _ n@(Name _ (Unique k) _), ty@(TyVar _ n')):tys)
    | n == n' = unifyPrep um tys -- a type variable is always equal to itself, don't bother inserting this!
    | otherwise = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys

unify :: [(KempeTy (), KempeTy ())] -> Either (Error ()) (IM.IntMap (KempeTy ()))
unify = unifyPrep IM.empty

unifyM :: S.Set (KempeTy (), KempeTy ()) -> TypeM () (IM.IntMap (KempeTy ()))
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
    pure $ StackType [TyVar () aN] []
typeOfBuiltin Swap = do
    aN <- dummyName "a"
    bN <- dummyName "b"
    pure $ StackType [TyVar () aN, TyVar () bN] [TyVar () bN, TyVar () aN]
typeOfBuiltin Dup = do
    aN <- dummyName "a"
    pure $ StackType [TyVar () aN] [TyVar () aN, TyVar () aN]
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
typeOfBuiltin IntGeq     = pure intRel
typeOfBuiltin IntNeq     = pure intRel
typeOfBuiltin IntGt      = pure intRel
typeOfBuiltin WordMinus  = pure wordBinOp
typeOfBuiltin WordDiv    = pure wordBinOp
typeOfBuiltin WordMod    = pure wordBinOp
typeOfBuiltin And        = pure boolOp
typeOfBuiltin Or         = pure boolOp
typeOfBuiltin Xor        = pure boolOp
typeOfBuiltin IntNeg     = pure $ StackType [TyBuiltin () TyInt] [TyBuiltin () TyInt]
typeOfBuiltin Popcount   = pure $ StackType [TyBuiltin () TyWord] [TyBuiltin () TyInt]

boolOp :: StackType ()
boolOp = StackType [TyBuiltin () TyBool, TyBuiltin () TyBool] [TyBuiltin () TyBool]

intRel :: StackType ()
intRel = StackType [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyBool]

intBinOp :: StackType ()
intBinOp = StackType [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyInt]

intShift :: StackType ()
intShift = StackType [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyInt]

wordBinOp :: StackType ()
wordBinOp = StackType [TyBuiltin () TyWord, TyBuiltin () TyWord] [TyBuiltin () TyWord]

wordShift :: StackType ()
wordShift = StackType [TyBuiltin () TyWord, TyBuiltin () TyWord] [TyBuiltin () TyWord]

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
dipify (StackType is os) = do
    n <- dummyName "a"
    pure $ StackType (is ++ [TyVar () n]) (os ++ [TyVar () n])

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
tyAtom BoolLit{}       = pure $ StackType [] [TyBuiltin () TyBool]
tyAtom IntLit{}        = pure $ StackType [] [TyBuiltin () TyInt]
tyAtom Int8Lit{}       = pure $ StackType [] [TyBuiltin () TyInt8 ]
tyAtom WordLit{}       = pure $ StackType [] [TyBuiltin () TyWord]
tyAtom (AtName _ n)    = renameStack =<< tyLookup (void n)
tyAtom (Dip _ as)      = dipify =<< tyAtoms as
tyAtom (AtCons _ tn)   = renameStack =<< consLookup (void tn)
tyAtom (If _ as as')   = do
    tys <- tyAtoms as
    tys' <- tyAtoms as'
    (StackType ins out) <- mergeStackTypes tys tys'
    pure $ StackType (ins ++ [TyBuiltin () TyBool]) out
tyAtom (Case _ ls) = do
    tyLs <- traverse tyLeaf ls
    -- TODO: one-pass fold?
    mergeMany tyLs

assignAtom :: Atom b a -> TypeM () (StackType (), Atom (StackType ()) (StackType ()))
assignAtom (AtBuiltin _ b) = do { ty <- typeOfBuiltin b ; pure (ty, AtBuiltin ty b) }
assignAtom (BoolLit _ b)   =
    let sTy = StackType [] [TyBuiltin () TyBool]
        in pure (sTy, BoolLit sTy b)
assignAtom (IntLit _ i)    =
    let sTy = StackType [] [TyBuiltin () TyInt]
        in pure (sTy, IntLit sTy i)
assignAtom (Int8Lit _ i)    =
    let sTy = StackType [] [TyBuiltin () TyInt8]
        in pure (sTy, Int8Lit sTy i)
assignAtom (WordLit _ u)    =
    let sTy = StackType [] [TyBuiltin () TyWord]
        in pure (sTy, WordLit sTy u)
assignAtom (AtName _ n) = do
    sTy <- renameStack =<< tyLookup (void n)
    pure (sTy, AtName sTy (n $> sTy))
assignAtom (AtCons _ tn) = do
    sTy <- renameStack =<< consLookup (void tn)
    pure (sTy, AtCons sTy (tn $> sTy))
assignAtom (Dip _ as) = do { (as', ty) <- assignAtoms as ; tyDipped <- dipify ty ; pure (tyDipped, Dip tyDipped as') }
assignAtom (If _ as0 as1) = do
    (as0', tys) <- assignAtoms as0
    (as1', tys') <- assignAtoms as1
    (StackType ins out) <- mergeStackTypes tys tys'
    let resType = StackType (ins ++ [TyBuiltin () TyBool]) out
    pure (resType, If resType as0' as1')
assignAtom (Case _ ls) = do
    lRes <- traverse assignCase ls
    resType <- mergeMany (fst3 <$> lRes)
    let newLeaves = fmap dropFst lRes
    pure (resType, Case resType newLeaves)
    where dropFst (_, y, z) = (y, z)

assignAtoms :: [Atom b a] -> TypeM () ([Atom (StackType ()) (StackType ())], StackType ())
assignAtoms []  = pure ([], emptyStackType)
assignAtoms [a] = do
    (ty, a') <- assignAtom a
    pure ([a'], ty)
assignAtoms (a:as) = do
    (ty, a') <- assignAtom a
    (as', ty') <- assignAtoms as
    (a':as' ,) <$> catTypes ty ty'

tyAtoms :: [Atom b a] -> TypeM () (StackType ())
tyAtoms [] = pure emptyStackType
tyAtoms [a] = tyAtom a
tyAtoms (a:as) = do
    ty <- tyAtom a
    tys <- tyAtoms as
    catTypes ty tys

-- from size,
mkHKT :: Int -> Kind
mkHKT 0 = Star
mkHKT i = TyCons (mkHKT $ i - 1) Star

tyInsertLeaf :: Name b -- ^ type being declared
             -> S.Set (Name b) -> (TyName a, [KempeTy b]) -> TypeM () ()
tyInsertLeaf n@(Name _ (Unique k) _) vars (Name _ (Unique i) _, ins) | S.null vars =
    modifying constructorTypesLens (IM.insert i (voidStackType $ StackType ins [TyNamed undefined n])) *>
    modifying kindEnvLens (IM.insert k Star)
                                               | otherwise =
    let ty = voidStackType $ StackType ins [app (TyNamed undefined n) (S.toList vars)] in
    modifying constructorTypesLens (IM.insert i ty) *>
    modifying kindEnvLens (IM.insert k (mkHKT $ S.size vars))

assignTyLeaf :: Name b
             -> S.Set (Name b)
             -> (TyName a, [KempeTy b])
             -> TypeM () (TyName (StackType ()), [KempeTy ()])
assignTyLeaf n@(Name _ (Unique k) _) vars (tn@(Name _ (Unique i) _), ins) | S.null vars =
    let ty = voidStackType $ StackType ins [TyNamed undefined n] in
    modifying constructorTypesLens (IM.insert i ty) *>
    modifying kindEnvLens (IM.insert k Star) $>
    (tn $> ty, fmap void ins)
                                               | otherwise =
    let ty = voidStackType $ StackType ins [app (TyNamed undefined n) (S.toList vars)] in
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
    sig <- renameStack $ voidStackType $ StackType ins os
    (as, inferred) <- assignAtoms a
    reconcile <- mergeStackTypes sig inferred
    when (inferred `lessGeneral` sig) $
        throwError $ LessGeneral () sig inferred
    pure $ FunDecl reconcile (n $> reconcile) (void <$> ins) (void <$> os) as
assignDecl (ExtFnDecl _ n ins os cn) = do
    traverse_ kindOf (void <$> ins ++ os)
    unless (length os <= 1) $
        throwError $ InvalidCImport () (void n)
    let sig = voidStackType $ StackType ins os
    pure $ ExtFnDecl sig (n $> sig) (void <$> ins) (void <$> os) cn
assignDecl (Export _ abi n) = do
    ty@(StackType _ os) <- tyLookup (void n)
    unless (abi == Kabi || length os <= 1) $
        throwError $ InvalidCExport () (void n)
    Export ty abi <$> assignName n

-- don't need to rename cuz it's only for exports (in theory)
assignName :: Name a -> TypeM () (Name (StackType ()))
assignName n = do { ty <- tyLookup (void n) ; pure (n $> ty) }

tyHeader :: KempeDecl a c b -> TypeM () ()
tyHeader Export{} = pure ()
tyHeader (FunDecl _ (Name _ (Unique i) _) ins out _) = do
    let sig = voidStackType $ StackType ins out
    modifying tyEnvLens (IM.insert i sig)
tyHeader (ExtFnDecl _ n@(Name _ (Unique i) _) ins os _) = do
    unless (length os <= 1) $
        throwError $ InvalidCImport () (void n)
    unless (null $ freeVars (ins ++ os)) $
        throwError $ TyVarExt () (void n)
    let sig = voidStackType $ StackType ins os -- no free variables allowed in c functions
    modifying tyEnvLens (IM.insert i sig)
tyHeader TyDecl{} = pure ()

type Vars a = IM.IntMap (Name a)

-- TODO: do we want strict or lazy?
type EqState a = State (Vars a)

-- need to check stack types are less general up to "alpha-equivalence"
-- (implicit forall with every new var! in a stack type)
--
-- Basically the inferred type has to check against the type in the signature.
-- Which I'm not sure this does; it should be better-founded with an
-- explanation of why it works (I'm not sure it works)
lessGeneral :: StackType a -- ^ Inferred type
            -> StackType a -- ^ Type from signature
            -> Bool
lessGeneral (StackType is os) (StackType is' os') =
    flip evalState mempty $
        if il > il' || ol > ol'
            then (||) <$> lessGenerals trimIs is' <*> lessGenerals trimOs os'
            else (||) <$> lessGenerals is trimIs' <*> lessGenerals os trimOs'
    where il = length is
          il' = length is'
          ol = length os
          ol' = length os'
          trimIs = drop (il-il') is
          trimIs' = drop (il'-il) is'
          trimOs = drop (ol-ol') os
          trimOs' = drop (ol'-ol) os'

          lessGeneralAtom :: KempeTy a -> KempeTy a -> EqState a Bool
          lessGeneralAtom TyBuiltin{} TyVar{}                   = pure True
          lessGeneralAtom TyApp{} TyVar{}                       = pure True
          lessGeneralAtom (TyApp _ ty ty') (TyApp _ ty'' ty''') = (||) <$> lessGeneralAtom ty ty'' <*> lessGeneralAtom ty' ty''' -- lazy pattern match?
          lessGeneralAtom _ _                                   = pure False
          lessGenerals :: [KempeTy a] -> [KempeTy a] -> EqState a Bool
          lessGenerals [] []                                 = pure False
          lessGenerals ((TyVar _ n):tys) ((TyVar _ n'):tys') = do
                st <- get
                let i = unUnique $ unique n
                case IM.lookup i st of
                    Nothing ->
                        modify (IM.insert i n') *>
                        lessGenerals tys tys' -- we can skip checking ty `lessGeneral` ty' at the first site
                    Just n'' ->
                        (n'' /= n' ||) <$> lessGenerals tys tys'
          lessGenerals (ty:tys) (ty':tys')                  = (||) <$> lessGeneralAtom ty ty' <*> lessGenerals tys tys'

tyInsert :: KempeDecl a c b -> TypeM () ()
tyInsert (TyDecl _ tn ns ls) = traverse_ (tyInsertLeaf tn (S.fromList ns)) ls
tyInsert (FunDecl _ _ ins out as) = do
    traverse_ kindOf (void <$> ins ++ out) -- FIXME: this gives sketchy results?
    sig <- renameStack $ voidStackType $ StackType ins out
    inferred <- tyAtoms as
    _ <- mergeStackTypes sig inferred
    when (inferred `lessGeneral` sig) $
        throwError $ LessGeneral () sig inferred
tyInsert (ExtFnDecl _ _ ins outs _) = traverse_ kindOf (void <$> ins ++ outs)
tyInsert Export{} = pure ()

tyModule :: Declarations a c b -> TypeM () ()
tyModule m = traverse_ tyHeader m *> traverse_ tyInsert m

checkModule :: Declarations a c b -> TypeM () ()
checkModule m = tyModule m <* (unifyM =<< gets constraints)

assignModule :: Declarations a c b -> TypeM () (Declarations () (StackType ()) (StackType ()))
assignModule m = {-# SCC "assignModule" #-} do
    {-# SCC "tyHeader" #-} traverse_ tyHeader m
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
renameStack (StackType ins outs) = do
    newQs <- traverse withName (S.toList $ freeVars ins <> freeVars outs)
    let (_, localRenames) = unzip newQs
        newBinds = thread localRenames
    withTyState newBinds $
        StackType <$> traverse renameIn ins <*> traverse renameIn outs

mergeStackTypes :: StackType () -> StackType () -> TypeM () (StackType ())
mergeStackTypes st0@(StackType i0 o0) st1@(StackType i1 o1) = do
    let li0 = length i0
        li1 = length i1
        toExpand = max (abs (li0 - li1)) (abs (length o0 - length o1))

    (StackType ins os) <- (if li0 < li1 then expandType toExpand else pure) st0
    (StackType ins' os') <- (if li1 < li0 then expandType toExpand else pure) st1

    when ((length ins /= length ins') || (length os /= length os')) $
        throwError $ MismatchedLengths () st0 st1

    zipWithM_ pushConstraint ins ins'
    zipWithM_ pushConstraint os os'

    pure $ StackType ins os

tyPattern :: Pattern b a -> TypeM () (StackType ())
tyPattern PatternWildcard{} = do
    aN <- dummyName "a"
    pure $ StackType [TyVar () aN] []
tyPattern PatternInt{} = pure $ StackType [TyBuiltin () TyInt] []
tyPattern PatternBool{} = pure $ StackType [TyBuiltin () TyBool] []
tyPattern (PatternCons _ tn) = renameStack . flipStackType =<< consLookup (void tn)

assignPattern :: Pattern b a -> TypeM () (StackType (), Pattern (StackType ()) (StackType ()))
assignPattern (PatternInt _ i) =
    let sTy = StackType [TyBuiltin () TyInt] []
        in pure (sTy, PatternInt sTy i)
assignPattern (PatternBool _ i) =
    let sTy = StackType [TyBuiltin () TyBool] []
        in pure (sTy, PatternBool sTy i)
assignPattern (PatternCons _ tn) = do { ty <- renameStack . flipStackType =<< consLookup (void tn) ; pure (ty, PatternCons ty (tn $> ty)) }
assignPattern PatternWildcard{} = do
    aN <- dummyName "a"
    let resType = StackType [TyVar () aN] []
    pure (resType, PatternWildcard resType)

mergeMany :: NonEmpty (StackType ()) -> TypeM () (StackType ())
mergeMany (t :| ts) = foldM mergeStackTypes t ts

-- assumes they have been renamed...
pushConstraint :: Ord a => KempeTy a -> KempeTy a -> TypeM a ()
pushConstraint ty ty' =
    modifying constraintsLens (S.insert (ty, ty'))

expandType :: Int -> StackType () -> TypeM () (StackType ())
expandType n (StackType i o) = do
    newVars <- replicateM n (dummyName "a")
    let newTy = TyVar () <$> newVars
    pure $ StackType (newTy ++ i) (newTy ++ o)

substConstraints :: IM.IntMap (KempeTy a) -> KempeTy a -> KempeTy a
substConstraints _ ty@TyNamed{}                         = ty
substConstraints _ ty@TyBuiltin{}                       = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) =
    case IM.lookup k tys of
        Just ty'@TyVar{}       -> substConstraints (IM.delete k tys) ty' -- TODO: this is to prevent cyclic lookups: is it right?
        Just (TyApp l ty0 ty1) -> let tys' = IM.delete k tys in TyApp l (substConstraints tys' ty0) (substConstraints tys' ty1)
        Just ty'               -> ty'
        Nothing                -> ty
substConstraints tys (TyApp l ty ty')                   =
    TyApp l (substConstraints tys ty) (substConstraints tys ty')

substConstraintsStack :: IM.IntMap (KempeTy a) -> StackType a -> StackType a
substConstraintsStack tys (StackType is os) = {-# SCC "substConstraintsStack" #-}
    let is' = substConstraints tys <$> is
        os' = substConstraints tys <$> os
        in StackType is' os'

-- do renaming before this
-- | Given @x@ and @y@, return the 'StackType' of @x y@
catTypes :: StackType () -- ^ @x@
         -> StackType () -- ^ @y@
         -> TypeM () (StackType ())
catTypes st0@(StackType _ osX) (StackType insY osY) = do
    let lY = length insY
        lDiff = lY - length osX

    -- all of the "ins" of y have to come from x, so we expand x as needed
    (StackType insX osX') <- if lDiff > 0
        then expandType lDiff st0
        else pure st0

    -- zip the last (length insY) of osX' with insY
    zipWithM_ pushConstraint (drop (length osX' - lY) osX') insY -- TODO splitAt

    pure $ StackType insX (take (length osX' - lY) osX' ++ osY)
