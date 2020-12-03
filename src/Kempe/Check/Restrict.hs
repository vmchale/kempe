-- | Check that sum types have <256 constructors
module Kempe.Check.Restrict ( restrictConstructors
                            ) where

import           Data.Foldable.Ext
import           Kempe.AST
import           Kempe.Error       (Error (FatSumType))

restrictConstructors :: Module a c b -> Maybe (Error a)
restrictConstructors = foldMapAlternative restrictDecl

restrictDecl :: KempeDecl a c b -> Maybe (Error a)
restrictDecl FunDecl{}         = Nothing
restrictDecl ExtFnDecl{}       = Nothing
restrictDecl Export{}          = Nothing
restrictDecl (TyDecl l n _ ls) | length ls > 256 = Just (FatSumType l n)
                               | otherwise = Nothing
