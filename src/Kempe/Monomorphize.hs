{-# LANGUAGE TupleSections #-}

module Kempe.Monomorphize ( monomorphize
                          , closedModule
                          -- * For testing
                          , mkModuleMap
                          , closure
                          ) where

import qualified Data.IntMap  as IM
import           Data.Maybe   (mapMaybe)
import qualified Data.Set     as S
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique

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

monomorphize :: Module () (StackType ()) -> Module () MonoStackType
monomorphize = undefined

-- | Filter so that only the 'KempeDecl's necessary for exports are there
closedModule :: Module a b -> Module a b
closedModule m = map pickDecl roots
    where key = mkModuleMap m
          roots = S.toList $ closure (m, key)
          pickDecl (Name _ (Unique i) _) =
            case IM.lookup i key of
                Just decl -> decl
                Nothing   -> error "Internal error! module map should contain all names."

closure :: (Module a b, ModuleMap a b) -> S.Set (Name b)
closure (m, key) = loop roots
    where roots = S.fromList (exports m)
          loop ns =
            let res = foldMap step ns
                in if res == ns
                    then res -- test it doesn't bottom on cyclic lookups...
                    else ns <> loop (res S.\\ ns)
          step (Name _ (Unique i) _) =
            case IM.lookup i key of
                Just decl -> namesInDecl decl
                Nothing   -> error "Internal error! module map should contain all names."

namesInDecl :: KempeDecl a b -> S.Set (Name b)
namesInDecl TyDecl{}             = S.empty
namesInDecl ExtFnDecl{}          = S.empty
namesInDecl Export{}             = S.empty
namesInDecl (FunDecl _ _ _ _ as) = foldMap namesInAtom as

namesInAtom :: Atom a -> S.Set (Name a)
namesInAtom AtBuiltin{}   = S.empty
namesInAtom (If _ as as') = foldMap namesInAtom as <> foldMap namesInAtom as'
namesInAtom (Dip _ as)    = foldMap namesInAtom as
namesInAtom (AtName _ n)  = S.singleton n
namesInAtom (AtCons _ tn) = S.singleton tn
namesInAtom IntLit{}      = S.empty
namesInAtom BoolLit{}     = S.empty

exports :: Module a b -> [Name b]
exports = mapMaybe exportsDecl

exportsDecl :: KempeDecl a b -> Maybe (Name b)
exportsDecl (Export _ _ n) = Just n
exportsDecl _              = Nothing
