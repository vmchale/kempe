{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Kempe.Monomorphize ( closedModule
                          -- * For testing
                          , mkModuleMap
                          , closure
                          ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (StateT, evalStateT)
import qualified Data.IntMap          as IM
import           Data.Maybe           (mapMaybe)
import           Data.Semigroup       ((<>))
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique

-- | New function names, also max state threaded through.
type RenameEnv = (Int, IM.IntMap Int)

type MonoM = StateT RenameEnv (Either (Error ()))

runMonoM :: Int -> MonoM a -> Either (Error ()) a
runMonoM maxI = flip evalStateT (maxI, mempty)

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

tyEquiv :: StackType () -> MonoStackType -> Bool
tyEquiv (StackType _ is os) (is', os') =
    is == is' && os == os'

-- | Filter so that only the 'KempeDecl's necessary for exports are there
--
-- This will throw an exception on ill-typed programs.
closedModule :: Ord b => Module a b -> Module a b
closedModule m = fmap pickDecl roots
    where key = mkModuleMap m
          roots = S.toList $ closure (m, key)
          pickDecl (Name _ (Unique i) _, _) =
            case IM.lookup i key of
                Just decl -> decl
                Nothing   -> error "Internal error! module map should contain all names."


closure :: Ord b => (Module a b, ModuleMap a b) -> S.Set (Name b, b)
closure (m, key) = loop roots
    where roots = S.fromList (exports m)
          loop ns =
            let res = foldMap step (S.map fst ns)
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

{-
--- all names + their type at call site
type Uses = S.Set (Name (), StackType ()) -- should this contain only polymorphic stuff?

usesAtom :: Atom (StackType ()) -> Uses
usesAtom AtBuiltin{}                = mempty
usesAtom BoolLit{}                  = mempty
usesAtom IntLit{}                   = mempty
usesAtom (AtName _ n@(Name _ _ l))  = S.singleton (void n, l)
usesAtom (If _ as as')              = foldMap usesAtom as <> foldMap usesAtom as'
usesAtom (Dip _ as)                 = foldMap usesAtom as
usesAtom (AtCons _ tn@(Name _ _ l)) = S.singleton (void tn, l)

usesDecl :: KempeDecl () (StackType ()) -> Uses
usesDecl TyDecl{}             = mempty
usesDecl ExtFnDecl{}          = mempty
usesDecl (FunDecl _ _ _ _ as) = foldMap usesAtom as

uses :: Module () (StackType ()) -> Uses
uses = foldMap usesDecl

-- | Convert a 'StackType' of an 'ExtFnDecl' to a 'MonoStackType'
retypeExt :: StackType () -> Either (Error ()) MonoStackType
retypeExt (StackType qs is os) | S.null qs = Right (is, os)
                               | otherwise = Left $ TyVarExt ()

checkDecl :: KempeDecl () (StackType ()) -> Either (Error ()) (KempeDecl () MonoStackType)
checkDecl (ExtFnDecl l (Name t u l') is os cn) = ExtFnDecl <$> retypeExt l <*> (Name t u <$> retypeExt l') <*> pure is <*> pure os <*> pure cn
checkDecl (Export l abi (Name t u l'))         = Export <$> retypeExt l <*> pure abi <*> (Name t u <$> retypeExt l')

--- decide which versions we need?
checkExt :: Module () (StackType ()) -> Either (Error ()) (Module () MonoStackType)
checkExt = traverse checkDecl
-}
