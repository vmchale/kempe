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
closure _ = undefined

exports :: Module a b -> [Name b]
exports = mapMaybe exportsDecl

exportsDecl :: KempeDecl a b -> Maybe (Name b)
exportsDecl (Export _ _ n) = Just n
exportsDecl _              = Nothing
