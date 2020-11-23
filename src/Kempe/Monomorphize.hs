{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | This module is kind of half-assed. I don't have any references but it should work.
module Kempe.Monomorphize ( closedModule
                          , MonoM
                          , runMonoM
                          , flattenModule
                          , ConsAnn (..)
                          -- * Benchmark
                          , closure
                          , mkModuleMap
                          ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Extra        (mapMaybeM)
import           Control.Monad.State.Strict (StateT, gets, runStateT)
import           Data.Bifunctor             (second)
import           Data.Function              (on)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import           Data.List                  (find, groupBy, partition)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe, mapMaybe)
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Tuple.Extra           (fst3, snd3, thd3)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Monomorphize.Error   (tryMono)
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)

-- | New function names, keyed by name + specialized type
--
-- also max state threaded through.
data RenameEnv = RenameEnv { maxState :: Int
                           , fnEnv    :: M.Map (Unique, StackType ()) Unique
                           , consEnv  :: M.Map (Unique, StackType ()) (Unique, ConsAnn (StackType ()))
                           }

type MonoM = StateT RenameEnv (Either (Error ()))

maxStateLens :: Lens' RenameEnv Int
maxStateLens f s = fmap (\x -> s { maxState = x }) (f (maxState s))

consEnvLens :: Lens' RenameEnv (M.Map (Unique, StackType ()) (Unique, ConsAnn (StackType ())))
consEnvLens f s = fmap (\x -> s { consEnv = x }) (f (consEnv s))

fnEnvLens :: Lens' RenameEnv (M.Map (Unique, StackType ()) Unique)
fnEnvLens f s = fmap (\x -> s { fnEnv = x }) (f (fnEnv s))

runMonoM :: Int -> MonoM a -> Either (Error ()) (a, Int)
runMonoM maxI = fmap (second maxState) . flip runStateT (RenameEnv maxI mempty mempty)

freshName :: T.Text -> a -> MonoM (Name a)
freshName n ty = do
    pSt <- gets maxState
    Name n (Unique $ pSt + 1) ty
        <$ modifying maxStateLens (+1)

-- | A 'ModuleMap' is a map which retrives the 'KempeDecl' associated with
-- a given 'Name'
type ModuleMap a c b = IM.IntMap (KempeDecl a c b)

mkModuleMap :: Module a c b -> ModuleMap a c b
mkModuleMap = IM.fromList . concatMap toInt where
    toInt d@(FunDecl _ (Name _ (Unique i) _) _ _ _)   = [(i, d)]
    toInt d@(ExtFnDecl _ (Name _ (Unique i) _) _ _ _) = [(i, d)]
    toInt d@(TyDecl _ _ _ ds)                         =
        let us = unUnique . unique . fst <$> ds
            in (, d) <$> us
    toInt _                                           = []

squishTypeName :: BuiltinTy -> T.Text
squishTypeName TyPtr  = "ptr"
squishTypeName TyInt  = "int"
squishTypeName TyBool = "bool"
squishTypeName TyWord = "word"
squishTypeName TyInt8 = "int8"

squishType :: KempeTy a -> T.Text
squishType (TyBuiltin _ b)          = squishTypeName b
squishType (TyNamed _ (Name t _ _)) = T.toLower t
squishType TyVar{}                  = error "not meant to be monomorphized!"
squishType (TyTuple _ tys)          = foldMap squishType tys
squishType (TyApp _ ty ty')         = squishType ty <> squishType ty'

squishMonoStackType :: MonoStackType -> T.Text
squishMonoStackType (is, os) = foldMap squishType is <> "TT" <> foldMap squishType os

renamePattern :: Pattern (StackType ()) (StackType ()) -> MonoM (Pattern (ConsAnn (StackType ())) (StackType ()))
renamePattern = undefined

renameCase :: (Pattern (StackType ()) (StackType ()), [Atom (StackType ()) (StackType ())]) -> MonoM (Pattern (ConsAnn (StackType ())) (StackType ()), [Atom (ConsAnn (StackType ())) (StackType ())])
renameCase (p, as) = (,) <$> renamePattern p <*> traverse renameAtom as

renameAtom :: Atom (StackType ()) (StackType ()) -> MonoM (Atom (ConsAnn (StackType ())) (StackType ()))
renameAtom (AtBuiltin ty b)         = pure $ AtBuiltin ty b
renameAtom (If ty as as')           = If ty <$> traverse renameAtom as <*> traverse renameAtom as'
renameAtom (IntLit ty i)            = pure $ IntLit ty i
renameAtom (Int8Lit ty i)           = pure $ Int8Lit ty i
renameAtom (WordLit ty w)           = pure $ WordLit ty w
renameAtom (BoolLit ty b)           = pure $ BoolLit ty b
renameAtom (Dip ty as)              = Dip ty <$> traverse renameAtom as
renameAtom (AtName ty (Name t u l)) = do
    mSt <- gets fnEnv
    let u' = M.findWithDefault u (u, ty) mSt
    pure $ AtName ty (Name t u' l)
renameAtom (Case ty ls)             = Case ty <$> traverse renameCase ls
renameAtom (AtCons ty (Name t u _)) = do
    cSt <- gets consEnv
    let (u', ann) = M.findWithDefault (error "Internal error? unfound constructor") (u, ty) cSt
    pure $ AtCons ann (Name t u' ann)

renameDecl :: KempeDecl () (StackType ()) (StackType ()) -> MonoM (Maybe (KempeDecl () (ConsAnn (StackType ())) (StackType ())))
renameDecl (FunDecl l n is os as) = Just . FunDecl l n is os <$> traverse renameAtom as
renameDecl (Export ty abi (Name t u l)) = do
    mSt <- gets fnEnv
    let u' = M.findWithDefault (error "Shouldn't happen; might be user error or internal error") (u, ty) mSt
    pure $ Just $ Export ty abi (Name t u' l)
renameDecl (ExtFnDecl l n tys tys' b) = pure $ Just $ ExtFnDecl l n tys tys' b
renameDecl TyDecl{} = pure Nothing -- don't need to

-- | Call 'closedModule' and perform any necessary renamings
flattenModule :: Module () (StackType ()) (StackType ()) -> MonoM (Module () (ConsAnn (StackType ())) (StackType ()))
flattenModule = renameMonoM <=< closedModule

-- | To be called after 'closedModule'
renameMonoM :: Module () (StackType ()) (StackType ()) -> MonoM (Module () (ConsAnn (StackType ())) (StackType ()))
renameMonoM = mapMaybeM renameDecl

-- | Filter so that only the 'KempeDecl's necessary for exports are there, and
-- fan out top-level functions into all necessary specializations.
--
-- This will throw an exception on ill-typed programs.
--
-- The 'Module' returned will have to be renamed.
closedModule :: Module () (StackType ()) (StackType ()) -> MonoM (Module () (StackType ()) (StackType ()))
closedModule m = addExports <$> do
    { fn' <- traverse (uncurry specializeDecl . drop1) fnDecls
    ; ty' <- specializeTyDecls tyDecls
    ; pure (ty' ++ fn')
    }
    where addExports = (++ exportsOnly m)
          key = mkModuleMap m
          roots = S.toList $ closure (m, key)
          gatherDecl (n@(Name _ (Unique i) _), ty) = -- TODO: findWithDefault?
            case IM.lookup i key of
                Just decl -> (n, decl, ty)
                Nothing   -> error "Internal error! module map should contain all names."
          rootDecl = gatherDecl <$> roots
          drop1 ~(_, y, z) = (y, z)
          (tyDecls, fnDecls) = partition (isTyDecl . snd3) rootDecl
          isTyDecl TyDecl{} = True
          isTyDecl _        = False

-- group specializations by type name?
specializeTyDecls :: [(TyName (StackType ()), KempeDecl () (StackType ()) (StackType ()), StackType ())] -> MonoM [KempeDecl () (StackType ()) (StackType ())]
specializeTyDecls ds = traverse (uncurry mkTyDecl) processed
    where toMerge = groupBy ((==) `on` snd3) ds
          processed = fmap process toMerge
          process tyDs@((_, x, _):_) = (x, zip (fst3 <$> tyDs) (thd3 <$> tyDs))
          process []                 = error "Empty group!"

mkTyDecl :: KempeDecl () (StackType ()) (StackType ()) -> [(TyName (StackType ()), StackType ())] -> MonoM (KempeDecl () (StackType ()) (StackType ()))
mkTyDecl (TyDecl _ tn ns preConstrs) constrs = do
    renCons <- traverse (\(tn', ty) -> do { ty'@(is, _) <- tryMono ty ; (, is) <$> renamedCons (tn' $> ty') ty' (ConsAnn (szType ty') (getTag tn')) }) constrs
    pure $ TyDecl () tn ns renCons
    where indexAt p xs = fst $ fromMaybe (error "Internal error.") $ find (\(_, x) -> p x) (zip [0..] xs)
          getTag (Name _ u _) = indexAt (== u) preIxes
          preIxes = fmap (unique . fst) preConstrs
          szType (_, [o]) = 1 + size o
          szType _        = error "Internal error: ill-typed constructor."
mkTyDecl _ _ = error "Shouldn't happen."

specializeDecl :: KempeDecl () (StackType ()) (StackType ()) -> StackType () -> MonoM (KempeDecl () (StackType ()) (StackType ()))
specializeDecl (FunDecl _ n _ _ as) sty = do
    (Name t u newStackType@(StackType _ is os)) <- renamed n =<< tryMono sty
    pure $ FunDecl newStackType (Name t u newStackType) is os as
specializeDecl (ExtFnDecl l n tys tys' b) _ = pure $ ExtFnDecl l n tys tys' b
specializeDecl (Export l abi n) _           = pure $ Export l abi n
specializeDecl TyDecl{} _                   = error "Shouldn't happen."
-- leave exports and foreign imports alone (have to be monomorphic)

renamedCons :: TyName a -> MonoStackType -> (StackType () -> ConsAnn (StackType ())) -> MonoM (TyName (StackType ()))
renamedCons (Name t i _) sty@(is, os) fAnn = do
    let t' = t <> squishMonoStackType sty
    (Name _ j _) <- freshName t' sty
    let newStackType = StackType S.empty is os
        ann = fAnn newStackType
    modifying consEnvLens (M.insert (i, newStackType) (j, ann))
    pure (Name t' j newStackType)

-- | Insert a specialized rename.
renamed :: Name a -> MonoStackType -> MonoM (Name (StackType ()))
renamed (Name t i _) sty@(is, os) = do
    let t' = t <> squishMonoStackType sty
    (Name _ j _) <- freshName t' sty
    let newStackType = StackType S.empty is os
    modifying fnEnvLens (M.insert (i, newStackType) j)
    pure (Name t' j newStackType)

closure :: Ord b => (Module a b b, ModuleMap a b b) -> S.Set (Name b, b)
closure (m, key) = loop roots S.empty
    where roots = S.fromList (exports m)
          loop ns avoid =
            let res = foldMap (step . fst) (ns S.\\ avoid)
                in if res == ns
                    then res
                    else ns <> loop res (ns <> avoid)
          step (Name _ (Unique i) _) =
            case IM.lookup i key of
                Just decl -> namesInDecl decl
                Nothing   -> error "Internal error! module map should contain all names."

namesInDecl :: Ord b => KempeDecl a b b -> S.Set (Name b, b)
namesInDecl TyDecl{}             = S.empty
namesInDecl ExtFnDecl{}          = S.empty
namesInDecl Export{}             = S.empty
namesInDecl (FunDecl _ _ _ _ as) = foldMap namesInAtom as

namesInAtom :: Ord a => Atom a a -> S.Set (Name a, a)
namesInAtom AtBuiltin{}                = S.empty
namesInAtom (If _ as as')              = foldMap namesInAtom as <> foldMap namesInAtom as'
namesInAtom (Dip _ as)                 = foldMap namesInAtom as
namesInAtom (AtName _ n@(Name _ _ l))  = S.singleton (n, l)
namesInAtom (AtCons _ tn@(Name _ _ l)) = S.singleton (tn, l)
namesInAtom IntLit{}                   = S.empty
namesInAtom BoolLit{}                  = S.empty
namesInAtom Int8Lit{}                  = S.empty
namesInAtom WordLit{}                  = S.empty
namesInAtom (Case _ as)                = foldMap namesInAtom (foldMap snd as) -- don't need patterns since we're destructing them here?

exports :: Module a c b -> [(Name b, b)]
exports = mapMaybe exportsDecl

exportsOnly :: Module a c b -> Module a c b
exportsOnly = mapMaybe getExport where
    getExport d@Export{} = Just d
    getExport _          = Nothing

exportsDecl :: KempeDecl a c b -> Maybe (Name b, b)
exportsDecl (Export _ _ n@(Name _ _ l)) = Just (n, l)
exportsDecl _                           = Nothing
