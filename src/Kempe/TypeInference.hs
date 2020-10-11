{-# LANGUAGE OverloadedStrings #-}

module Kempe.TypeInference ( TypeM
                           , runTypeM
                           , tyAtoms
                           , tyInsert
                           -- * Exported for testing
                           , renameStack
                           ) where

import           Control.Composition  (thread)
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Control.Monad.State
import           Data.Foldable        (traverse_)
import qualified Data.IntMap          as IM
import           Data.List.NonEmpty   (NonEmpty (..))
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

dummyName :: T.Text -> TypeM () (Name ())
dummyName n = do
    pSt <- gets maxU
    Name n (Unique $ pSt + 1) ()
        <$ modifying maxULens (+1)

type TypeM a = ExceptT (Error a) (State (TyState a))

-- TODO: take constructor types as an argument?..
runTypeM :: Int -> TypeM a x -> Either (Error a) x
runTypeM maxInt = flip evalState (TyState maxInt mempty mempty mempty S.empty) . runExceptT

-- alpha-equivalence (of 'StackType's?) (note it is quantified *only* on the "exterior" i.e.
-- implicitly) -> except we have to then "back-instantiate"? hm

-- monomorphization

-- dip-ify?

-- renameStackType? or maybe j substitute?

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

-- maybe constraints? e.g. ("a" = "b") and (3 = "a")
-- but maybe simpler since no function types? lol
--
-- so I can literally just check it's 3 and then pass that back lololol
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

dipify :: StackType () -> TypeM () (StackType ())
dipify (StackType fvrs is os) = do
    n <- dummyName "a"
    pure $ StackType (S.insert n fvrs) (TyNamed () n:is) (TyNamed () n:os)

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
    (\seed a -> do { tys' <- tyAtom a ; catTypes tys' seed })
    emptyStackType

tyInsertLeaf :: Name a -- ^ type being declared
             -> S.Set (Name a) -> (TyName a, [KempeTy a]) -> TypeM () ()
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

tyInsert :: KempeDecl a -> TypeM () ()
tyInsert (TyDecl _ tn ns ls) = traverse_ (tyInsertLeaf tn (S.fromList ns)) ls
tyInsert (FunDecl _ (Name _ (Unique i) _) ins out as) = do
    let sig = voidStackType $ StackType (freeVars (ins ++ out)) ins out
    inferred <- tyAtoms as
    reconcile <- mergeStackTypes sig inferred
    modifying tyEnvLens (IM.insert i reconcile) -- lel microlens-tardis

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

-- dispatch constraints?
mergeStackTypes :: StackType () -> StackType () -> TypeM () (StackType ())
mergeStackTypes st0 st1 = do
    -- freshen stack types (free vars) so no clasing/overwriting happens
    (StackType q _ _) <- renameStack st0
    (StackType q' _ _) <- renameStack st1
    pure $ StackType (q <> q') undefined undefined

tyPattern :: Pattern a -> TypeM () (S.Set (Name ()), [KempeTy ()])
tyPattern PatternWildcard{} = do
    aN <- dummyName "a"
    pure (S.singleton aN, [TyVar () aN])
tyPattern PatternInt{} = pure (S.empty, [TyBuiltin () TyInt])
tyPattern PatternBool{} = pure (S.empty, [TyBuiltin () TyBool])

mergeMany :: NonEmpty (StackType ()) -> TypeM () (StackType ())
mergeMany (t :| ts) = foldM mergeStackTypes t ts

-- | Given @x@ and @y@, return the 'StackType' of @x y@
catTypes :: StackType a -- ^ @x@
         -> StackType a -- ^ @y@
         -> TypeM () (StackType ())
catTypes _ _ = pure undefined -- I need unification? :o
-- all of the "ins" of y have to come from x
