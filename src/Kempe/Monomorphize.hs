{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Kempe.Monomorphize ( closedModule
                          , MonoM
                          , runMonoM
                          ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (StateT, evalStateT, gets)
import qualified Data.IntMap          as IM
import qualified Data.Map             as M
import           Data.Maybe           (mapMaybe)
import           Data.Semigroup       ((<>))
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro           (_1, _2)
import           Lens.Micro.Mtl       (modifying)

-- | New function names, keyed by name + specialized type
--
-- also max state threaded through.
type RenameEnv = (Int, M.Map (Unique, MonoStackType) Unique)

type MonoM = StateT RenameEnv (Either (Error ()))

runMonoM :: Int -> MonoM a -> Either (Error ()) a
runMonoM maxI = flip evalStateT (maxI, mempty)

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

type MonoStackType = ([KempeTy ()], [KempeTy ()])

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

tryMono :: StackType () -> MonoM MonoStackType
tryMono (StackType _ is os) | S.null (freeVars (is ++ os)) = pure (is, os)
                            | otherwise = throwError $ MonoFailed ()

-- | Filter so that only the 'KempeDecl's necessary for exports are there.
--
-- This will throw an exception on ill-typed programs.
--
-- The 'Module' returned will still have to be renamed.
closedModule :: Module () (StackType ()) -> MonoM (Module () (StackType ()))
closedModule m = traverse pickDecl roots
    where key = mkModuleMap m
          roots = S.toList $ closure (m, key)
          pickDecl (Name _ (Unique i) _, ty) =
            case IM.lookup i key of
                Just decl -> specializeDecl decl ty
                Nothing   -> error "Internal error! module map should contain all names."

specializeDecl :: KempeDecl () (StackType ()) -> StackType () -> MonoM (KempeDecl () (StackType ()))
specializeDecl (FunDecl _ n _ _ as) sty = do
    mTy <- tryMono sty
    (Name t u (is, os)) <- renamed n mTy
    let newStackType = StackType S.empty is os
    pure $ FunDecl newStackType (Name t u newStackType) is os as

{-
-- | Convert a 'StackType' of an 'ExtFnDecl' to a 'MonoStackType'
retypeExt :: StackType () -> MonoM MonoStackType
retypeExt (StackType qs is os) | S.null qs = pure (is, os)
                               | otherwise = throwError $ TyVarExt ()

checkDecl :: KempeDecl () (StackType ()) -> Either (Error ()) (KempeDecl () MonoStackType)
checkDecl (ExtFnDecl l (Name t u l') is os cn) = ExtFnDecl <$> retypeExt l <*> (Name t u <$> retypeExt l') <*> pure is <*> pure os <*> pure cn
checkDecl (Export l abi (Name t u l'))         = Export <$> retypeExt l <*> pure abi <*> (Name t u <$> retypeExt l')
-}

-- | Insert a specialized rename.
renamed :: Name a -> MonoStackType -> MonoM (Name MonoStackType)
renamed (Name t i _) sty = do
    let t' = t <> squishMonoStackType sty
    newTA@(Name _ j _) <- freshName t' sty
    modifying _2 (M.insert (i, sty) j)
    pure newTA

closure :: Ord b => (Module a b, ModuleMap a b) -> S.Set (Name b, b)
closure (m, key) = loop roots
    where roots = S.fromList (exports m)
          loop ns =
            let res = foldMap (step . fst) ns
                in if res == ns
                    then res -- test it doesn't bottom on cyclic lookups...
                    else ns <> loop (res S.\\ ns)
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

exports :: Module a b -> [(Name b, b)]
exports = mapMaybe exportsDecl

exportsDecl :: KempeDecl a b -> Maybe (Name b, b)
exportsDecl (Export _ _ n@(Name _ _ l)) = Just (n, l)
exportsDecl _                           = Nothing
