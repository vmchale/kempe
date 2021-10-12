module Kempe.CGen ( cGen
                  ) where

import           Data.Maybe     (mapMaybe)
import           Kempe.AST
import           Language.C.AST

cGen :: Declarations a c (StackType ()) -> [CFunc]
cGen = mapMaybe cDecl

cDecl :: KempeDecl a c (StackType ()) -> Maybe CFunc
cDecl ExtFnDecl{} = Nothing
cDecl TyDecl{}    = Nothing
cDecl FunDecl{}   = Nothing
