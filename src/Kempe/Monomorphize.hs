{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | This module is kind of half-assed. I don't have any references but it should work.
module Kempe.Monomorphize ( closedModule
                          , MonoM
                          , runMonoM
                          , flattenModule
                          , tryMono
                          -- * Benchmark
                          , closure
                          , mkModuleMap
                          ) where

import           Control.Monad              ((<=<))
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.State.Strict (StateT, gets, runStateT)
import           Data.Bifunctor             (second)
import           Data.Functor               ((<&>))
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import           Data.Maybe                 (mapMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (_1, _2)
import           Lens.Micro.Mtl             (modifying)

-- | New function names, keyed by name + specialized type
--
-- also max state threaded through.
type RenameEnv = (Int, M.Map (Unique, StackType ()) Unique)

type MonoM = StateT RenameEnv (Either (Error ()))

runMonoM :: Int -> MonoM a -> Either (Error ()) (a, Int)
runMonoM maxI = fmap (second fst) . flip runStateT (maxI, mempty)

freshName :: T.Text -> a -> MonoM (Name a)
freshName n ty = do
    pSt <- gets fst
    Name n (Unique $ pSt + 1) ty
        <$ modifying _1 (+1)

-- | A 'ModuleMap' is a map which retrives the 'KempeDecl' associated with
-- a given 'Name'
type ModuleMap a b = IM.IntMap (KempeDecl a b)

mkModuleMap :: Module a b -> ModuleMap a b
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

squishType :: KempeTy a -> T.Text
squishType (TyBuiltin _ b)          = squishTypeName b
squishType (TyNamed _ (Name t _ _)) = T.toLower t
squishType TyVar{}                  = error "not meant to be monomorphized!"
squishType (TyTuple _ tys)          = foldMap squishType tys
squishType (TyApp _ ty ty')         = squishType ty <> squishType ty'

squishMonoStackType :: MonoStackType -> T.Text
squishMonoStackType (is, os) = foldMap squishType is <> "TT" <> foldMap squishType os

tryMono :: MonadError (Error ()) m => StackType () -> m MonoStackType
tryMono (StackType _ is os) | S.null (freeVars (is ++ os)) = pure (is, os)
                            | otherwise = throwError $ MonoFailed ()

renameCase :: (Pattern (StackType ()), [Atom (StackType ())]) -> MonoM (Pattern (StackType ()), [Atom (StackType ())])
renameCase (p, as) = (p,) <$> traverse renameAtom as

renameAtom :: Atom (StackType ()) -> MonoM (Atom (StackType ()))
renameAtom a@AtBuiltin{}            = pure a
renameAtom (If ty as as')           = If ty <$> traverse renameAtom as <*> traverse renameAtom as'
renameAtom a@IntLit{}               = pure a
renameAtom a@BoolLit{}              = pure a
renameAtom (Dip ty as)              = Dip ty <$> traverse renameAtom as
renameAtom (AtName ty (Name t u l)) = do
    mSt <- gets snd
    let u' = M.findWithDefault u (u, ty) mSt
    pure $ AtName ty (Name t u' l)
renameAtom (Case ty ls)             = Case ty <$> traverse renameCase ls

renameDecl :: KempeDecl () (StackType ()) -> MonoM (KempeDecl () (StackType ()))
renameDecl (FunDecl l n is os as) = FunDecl l n is os <$> traverse renameAtom as
renameDecl (Export ty abi (Name t u l)) = do
    mSt <- gets snd
    let u' = M.findWithDefault (error "Shouldn't happen") (u, ty) mSt
    pure $ Export ty abi (Name t u' l)
renameDecl d@ExtFnDecl{}          = pure d

-- | Call 'closedModule' and perform any necessary renamings
flattenModule :: Module () (StackType ()) -> MonoM (Module () (StackType ()))
flattenModule = renameMonoM <=< closedModule

-- | To be called after 'closedModule'
renameMonoM :: Module () (StackType ()) -> MonoM (Module () (StackType ()))
renameMonoM = traverse renameDecl

-- | Filter so that only the 'KempeDecl's necessary for exports are there, and
-- fan out top-level functions into all necessary specializations.
--
-- This will throw an exception on ill-typed programs.
--
-- The 'Module' returned will have to be renamed.
closedModule :: Module () (StackType ()) -> MonoM (Module () (StackType ()))
closedModule m = traverse pickDecl roots <&> addExports
    where addExports = (exportsOnly m ++)
          key = mkModuleMap m
          roots = S.toList $ closure (m, key)
          pickDecl (Name _ (Unique i) _, ty) = -- TODO: findWithDefault?
            case IM.lookup i key of
                Just decl -> specializeDecl decl ty
                Nothing   -> error "Internal error! module map should contain all names."

specializeDecl :: KempeDecl () (StackType ()) -> StackType () -> MonoM (KempeDecl () (StackType ()))
specializeDecl (FunDecl _ n _ _ as) sty = do
    (Name t u newStackType@(StackType _ is os)) <- renamed n =<< tryMono sty
    pure $ FunDecl newStackType (Name t u newStackType) is os as
specializeDecl d@ExtFnDecl{} _ = pure d
specializeDecl d@Export{} _    = pure d
-- leave exports and foreign imports alone (have to be monomorphic)

-- | Insert a specialized rename.
renamed :: Name a -> MonoStackType -> MonoM (Name (StackType ()))
renamed (Name t i _) sty@(is, os) = do
    let t' = t <> squishMonoStackType sty
    (Name _ j _) <- freshName t' sty
    let newStackType = StackType S.empty is os
    modifying _2 (M.insert (i, newStackType) j)
    pure (Name t' j newStackType)

closure :: Ord b => (Module a b, ModuleMap a b) -> S.Set (Name b, b)
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

namesInDecl :: Ord b => KempeDecl a b -> S.Set (Name b, b)
namesInDecl TyDecl{}             = S.empty
namesInDecl ExtFnDecl{}          = S.empty
namesInDecl Export{}             = S.empty
namesInDecl (FunDecl _ _ _ _ as) = foldMap namesInAtom as

namesInAtom :: Ord a => Atom a -> S.Set (Name a, a)
namesInAtom AtBuiltin{}                = S.empty
namesInAtom (If _ as as')              = foldMap namesInAtom as <> foldMap namesInAtom as'
namesInAtom (Dip _ as)                 = foldMap namesInAtom as
namesInAtom (AtName _ n@(Name _ _ l))  = S.singleton (n, l)
namesInAtom (AtCons _ tn@(Name _ _ l)) = S.singleton (tn, l)
namesInAtom IntLit{}                   = S.empty
namesInAtom BoolLit{}                  = S.empty
namesInAtom (Case _ as)                = foldMap namesInAtom (foldMap snd as) -- don't need patterns since we're destructing them here?

exports :: Module a b -> [(Name b, b)]
exports = mapMaybe exportsDecl

exportsOnly :: Module a b -> Module a b
exportsOnly = mapMaybe getExport where
    getExport d@Export{} = Just d
    getExport _          = Nothing

exportsDecl :: KempeDecl a b -> Maybe (Name b, b)
exportsDecl (Export _ _ n@(Name _ _ l)) = Just (n, l)
exportsDecl _                           = Nothing
