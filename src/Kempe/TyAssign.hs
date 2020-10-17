{-# LANGUAGE OverloadedStrings #-}

module Kempe.TyAssign ( TypeM
                      , runTypeM
                      , checkModule
                      , assignModule
                      ) where

import           Control.Composition  (thread)
import           Control.Monad        (foldM, replicateM, when, zipWithM_)
import           Control.Monad.Except (throwError)
import           Control.Monad.State  (StateT, evalStateT, get, gets, modify, put)
import           Data.Foldable        (traverse_)
import           Data.Functor         (void, ($>))
import qualified Data.IntMap          as IM
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro           (Lens', over)
import           Lens.Micro.Mtl       (modifying, (.=))

type TyEnv a = IM.IntMap (StackType a)

data TyState a = TyState { maxU             :: Int -- ^ For renamer
                         , tyEnv            :: TyEnv a
                         , renames          :: IM.IntMap Int
                         , constructorTypes :: IM.IntMap (StackType a)
                         , constraints      :: S.Set (KempeTy a, KempeTy a) -- Just need equality between simple types? (do have tyapp but yeah)
                         }

emptyStackType :: StackType a
emptyStackType = StackType mempty [] []

maxULens :: Lens' (TyState a) Int
maxULens f s = fmap (\x -> s { maxU = x }) (f (maxU s))

constructorTypesLens :: Lens' (TyState a) (IM.IntMap (StackType a))
constructorTypesLens f s = fmap (\x -> s { constructorTypes = x }) (f (constructorTypes s))

tyEnvLens :: Lens' (TyState a) (TyEnv a)
tyEnvLens f s = fmap (\x -> s { tyEnv = x }) (f (tyEnv s))

renamesLens :: Lens' (TyState a) (IM.IntMap Int)
renamesLens f s = fmap (\x -> s { renames = x }) (f (renames s))

constraintsLens :: Lens' (TyState a) (S.Set (KempeTy a, KempeTy a))
constraintsLens f s = fmap (\x -> s { constraints = x }) (f (constraints s))

dummyName :: T.Text -> TypeM () (Name ())
dummyName n = do
    pSt <- gets maxU
    Name n (Unique $ pSt + 1) ()
        <$ modifying maxULens (+1)

type TypeM a = StateT (TyState a) (Either (Error a))

onType :: (Int, KempeTy a) -> KempeTy a -> KempeTy a
onType _ ty'@TyBuiltin{} = ty'
onType _ ty'@TyNamed{}   = ty'
onType (k, ty) ty'@(TyVar _ (Name _ (Unique i) _)) | i == k = ty
                                                    | otherwise = ty'

renameForward :: (Int, KempeTy a) -> [(KempeTy a, KempeTy a)] -> [(KempeTy a, KempeTy a)]
renameForward _ []                      = []
renameForward (k, ty) ((ty', ty''):tys) = (onType (k, ty) ty', onType (k, ty) ty'') : renameForward (k, ty) tys

unify :: [(KempeTy a, KempeTy a)] -> Either (Error ()) (IM.IntMap (KempeTy ()))
unify []                                                            = Right mempty
unify ((ty@(TyBuiltin _ b0), ty'@(TyBuiltin _ b1)):tys) | b0 == b1  = unify tys
                                                        | otherwise = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@(TyNamed _ n0), ty'@(TyNamed _ n1)):tys) | n0 == n1      = unify tys
                                                    | otherwise     = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@(TyNamed _ _), TyVar  _ (Name _ (Unique k) _)):tys)      = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyNamed _ _)):tys)       = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((ty@(TyBuiltin _ _), TyVar  _ (Name _ (Unique k) _)):tys)    = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyBuiltin _ _)):tys)     = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((TyVar _ (Name _ (Unique k) _), ty@(TyVar _ _)):tys)         = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((ty@TyBuiltin{}, ty'@TyNamed{}):_)                           = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyNamed{}, ty'@TyBuiltin{}):_)                           = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyBuiltin{}, ty'@TyApp{}):_)                             = Left (UnificationFailed () (void ty) (void ty'))
unify ((ty@TyNamed{}, ty'@TyApp{}):_)                               = Left (UnificationFailed () (void ty) (void ty'))
unify ((TyVar _ (Name _ (Unique k) _), ty@TyApp {}):tys)       = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)
unify ((ty@TyApp {}, TyVar  _ (Name _ (Unique k) _)):tys)      = IM.insert k (void ty) <$> unify (renameForward (k, ty) tys)

unifyM :: S.Set (KempeTy a, KempeTy a) -> TypeM () (IM.IntMap (KempeTy ()))
unifyM s =
    case unify (S.toList s) of
        Right x  -> pure x
        Left err -> throwError err

-- TODO: take constructor types as an argument?..
runTypeM :: Int -- ^ For renamer
         -> TypeM a x -> Either (Error a) x
runTypeM maxInt =
    flip evalStateT (TyState maxInt mempty mempty mempty S.empty)

-- monomorphization

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
typeOfBuiltin IntEq    = pure $ StackType S.empty [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyBool]
typeOfBuiltin IntMod   = pure intBinOp
typeOfBuiltin IntDiv   = pure intBinOp
typeOfBuiltin IntPlus  = pure intBinOp
typeOfBuiltin IntTimes = pure intBinOp
typeOfBuiltin IntMinus = pure intBinOp

intBinOp :: StackType ()
intBinOp = StackType S.empty [TyBuiltin () TyInt, TyBuiltin () TyInt] [TyBuiltin () TyInt]

tyLookup :: Name a -> TypeM a (StackType a)
tyLookup n@(Name _ (Unique i) l) = do
    st <- gets tyEnv
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError (PoorScope l n)

consLookup :: TyName a -> TypeM a (StackType a)
consLookup tn@(Name _ (Unique i) l) = do
    st <- gets constructorTypes
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError (PoorScope l tn)

-- expandType 1
dipify :: StackType () -> TypeM () (StackType ())
dipify (StackType fvrs is os) = do
    n <- dummyName "a"
    pure $ StackType (S.insert n fvrs) (TyNamed () n:is) (TyNamed () n:os)

assignAtom :: Atom a -> TypeM () (Atom (StackType ()))
assignAtom a@(AtBuiltin _ b) = AtBuiltin <$> tyAtom a <*> pure b
assignAtom a@(BoolLit _ b)   = BoolLit <$> tyAtom a <*> pure b
assignAtom a@(IntLit _ i)    = IntLit <$> tyAtom a <*> pure i
assignAtom a@(AtName _ n)    = AtName <$> tyAtom a <*> assignName n
assignAtom a@(Dip _ as)      = Dip <$> tyAtom a <*> traverse assignAtom as
assignAtom a@(AtCons _ tn)   = AtCons <$> tyAtom a <*> assignName tn
assignAtom a@(If _ as as')   = If <$> tyAtom a <*> traverse assignAtom as <*> traverse assignAtom as'

assignName :: Name a -> TypeM () (Name (StackType ()))
assignName n = do { ty <- tyLookup (void n) ; pure (n $> ty) }

assignCons :: Name a -> TypeM () (TyName (StackType ()))
assignCons n = do { ty <- consLookup (void n) ; pure (n $> ty) }

tyAtom :: Atom a -> TypeM () (StackType ())
tyAtom (AtBuiltin _ b) = typeOfBuiltin b
tyAtom BoolLit{}       = pure $ StackType mempty [] [TyBuiltin () TyBool]
tyAtom IntLit{}        = pure $ StackType mempty [] [TyBuiltin () TyInt]
tyAtom (AtName _ n)    = tyLookup (void n)
tyAtom (Dip _ as)      = dipify =<< tyAtoms as
tyAtom (AtCons _ tn)   = consLookup (void tn)
tyAtom (If _ as as')   = do
    tys <- tyAtoms as
    tys' <- tyAtoms as'
    (StackType vars ins out) <- mergeStackTypes tys tys'
    pure $ StackType vars (TyBuiltin () TyBool:ins) out

tyAtoms :: [Atom a] -> TypeM () (StackType ())
tyAtoms = foldM
    (\seed a -> do { tys' <- renameStack =<< tyAtom a ; catTypes tys' seed })
    emptyStackType

tyInsertLeaf :: Name b -- ^ type being declared
             -> S.Set (Name b) -> (TyName a, [KempeTy b]) -> TypeM () ()
tyInsertLeaf n vars (Name _ (Unique i) _, ins) =
    modifying constructorTypesLens (IM.insert i (voidStackType $ StackType vars ins [TyNamed undefined n]))

extrVars :: KempeTy a -> [Name a]
extrVars TyBuiltin{}      = []
extrVars TyNamed{}        = []
extrVars (TyVar _ n)      = [n]
extrVars (TyApp _ ty ty') = extrVars ty ++ extrVars ty'
extrVars (TyTuple _ tys)  = concatMap extrVars tys

freeVars :: [KempeTy a] -> S.Set (Name a)
freeVars tys = S.fromList (concatMap extrVars tys)

assignLeaf :: (TyName a, [KempeTy b]) -> TypeM () (TyName (StackType ()), [KempeTy ()])
assignLeaf (tn, tys) = (,) <$> assignCons tn <*> pure (void <$> tys)

assignDecl :: KempeDecl a b -> TypeM () (KempeDecl () (StackType ()))
assignDecl (TyDecl _ tn ns ls) = TyDecl () (void tn) (void <$> ns) <$> traverse assignLeaf ls
assignDecl (FunDecl _ n ins os a) = do
    ty <- tyLookup (void n)
    FunDecl ty <$> assignName n <*> pure (void <$> ins) <*> pure (void <$> os) <*> traverse assignAtom a
assignDecl (ExtFnDecl _ n ins os cn) = do
    ty <- tyLookup (void n)
    ExtFnDecl ty <$> assignName n <*> pure (void <$> ins) <*> pure (void <$> os) <*> pure cn
assignDecl (Export _ abi n) = do
    ty <- tyLookup (void n)
    Export ty abi <$> assignName n

-- TODO: traverse headers first
tyInsert :: KempeDecl a b -> TypeM () ()
tyInsert (TyDecl _ tn ns ls) = traverse_ (tyInsertLeaf tn (S.fromList ns)) ls
tyInsert (FunDecl _ (Name _ (Unique i) _) ins out as) = do
    let sig = voidStackType $ StackType (freeVars (ins ++ out)) ins out
    inferred <- tyAtoms as
    reconcile <- mergeStackTypes sig inferred -- FIXME: need to verify the merged type is as general as the signature!
    modifying tyEnvLens (IM.insert i reconcile)
tyInsert (ExtFnDecl _ (Name _ (Unique i) _) ins os _) = do
    sig <- renameStack $ voidStackType $ StackType S.empty ins os -- no free variables allowed in c functions
    modifying tyEnvLens (IM.insert i sig)
tyInsert Export{} = pure ()

checkModule :: Module a b -> TypeM () ()
checkModule m = traverse_ tyInsert m <* (unifyM =<< gets constraints)

assignModule :: Module a b -> TypeM () (Module () (StackType ()))
assignModule m = do
    traverse_ tyInsert m
    backNames <- unifyM =<< gets constraints
    fmap (fmap (substConstraintsStack backNames)) <$> traverse assignDecl m

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
renameIn (TyTuple l tys)  = TyTuple l <$> traverse renameIn tys
renameIn (TyVar l (Name t u l')) = do
    u' <- replaceUnique u
    pure $ TyVar l (Name t u' l')

-- has to use the max-iest maximum so we can't use withState
withTyState :: (TyState a -> TyState a) -> TypeM a (StackType a) -> TypeM a (StackType a)
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
    let localRenames = snd <$> newQs
        newNames = fst <$> newQs
        newBinds = thread localRenames
    withTyState newBinds $
        StackType (S.fromList newNames) <$> traverse renameIn ins <*> traverse renameIn outs

mergeStackTypes :: StackType () -> StackType () -> TypeM () (StackType ())
mergeStackTypes st0@(StackType _ i0 o0) st1@(StackType _ i1 o1) = do
    let toExpand = max (abs (length i0 - length i1)) (abs (length o0 - length o1))

    -- freshen stack types (free vars) so no clashing/overwriting happens
    (StackType q ins os) <- expandType toExpand =<< renameStack st0
    (StackType q' ins' os') <- expandType toExpand =<< renameStack st1

    when ((length ins /= length ins') || (length os /= length os')) $
        throwError $ MismatchedLengths () st0 st1

    zipWithM_ pushConstraint ins ins'
    zipWithM_ pushConstraint os os'

    pure $ StackType (q <> q') ins os

tyPattern :: Pattern a -> TypeM () (S.Set (Name ()), [KempeTy ()]) -- TODO: should this be a StackType for ease of use?
tyPattern PatternWildcard{} = do
    aN <- dummyName "a"
    pure (S.singleton aN, [TyVar () aN])
tyPattern PatternInt{} = pure (S.empty, [TyBuiltin () TyInt])
tyPattern PatternBool{} = pure (S.empty, [TyBuiltin () TyBool])

mergeMany :: NonEmpty (StackType ()) -> TypeM () (StackType ())
mergeMany (t :| ts) = foldM mergeStackTypes t ts

-- assumes they have been renamed...
pushConstraint :: KempeTy a -> KempeTy a -> TypeM () ()
pushConstraint ty ty' =
    modifying constraintsLens (S.insert (void ty, void ty'))

expandType :: Int -> StackType () -> TypeM () (StackType ())
expandType n (StackType q i o) = do
    newVars <- replicateM n (dummyName "a")
    let newTy = TyVar () <$> newVars
    pure $ StackType (q <> S.fromList newVars) (newTy ++ i) (newTy ++ o)

substConstraints :: IM.IntMap (KempeTy a) -> KempeTy a -> KempeTy a
substConstraints _ ty@TyNamed{}                         = ty
substConstraints _ ty@TyBuiltin{}                       = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) =
    fromMaybe ty (IM.lookup k tys)

substConstraintsStack :: IM.IntMap (KempeTy a) -> StackType a -> StackType a
substConstraintsStack tys (StackType _ is os) =
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
