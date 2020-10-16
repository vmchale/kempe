module Kempe.Monomorphize ( exports
                          , mkModuleMap
                          ) where

import qualified Data.IntMap  as IM
import           Data.Maybe   (mapMaybe)
import qualified Data.Set     as S
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique

type ModuleMap a b = IM.IntMap (KempeDecl a b)

mkModuleMap :: Module a b -> ModuleMap a b
mkModuleMap = IM.fromList . mapMaybe toInt where
    toInt d@(FunDecl _ (Name _ (Unique i) _) _ _ _)   = Just (i, d)
    toInt d@(ExtFnDecl _ (Name _ (Unique i) _) _ _ _) = Just (i, d)
    toInt _                                           = Nothing

closure :: Module a b -> S.Set (Name b)
closure m = undefined
    where roots = exports m

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

exports :: Module a b -> [Name b]
exports = mapMaybe exportsDecl

exportsDecl :: KempeDecl a b -> Maybe (Name b)
exportsDecl (Export _ _ n) = Just n
exportsDecl _              = Nothing
