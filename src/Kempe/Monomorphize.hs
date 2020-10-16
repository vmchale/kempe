module Kempe.Monomorphize ( exports
                          ) where

import           Data.Maybe (mapMaybe)
import qualified Data.Set   as S
import           Kempe.AST
import           Kempe.Name

exports :: Module a b -> S.Set (Name b)
exports = S.fromList . mapMaybe exportsDecl

exportsDecl :: KempeDecl a b -> Maybe (Name b)
exportsDecl (Export _ _ n) = Just n
exportsDecl _              = Nothing
