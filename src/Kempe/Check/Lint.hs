module Kempe.Check.Lint ( lint
                        ) where

import           Data.Foldable.Ext
import           Kempe.AST
import           Kempe.Error.Warning

lint :: Declarations a b b -> Maybe (Warning b)
lint = foldMapAlternative lintDecl

-- TODO: lint for something like dip(0) -> replace with 0 swap

lintDecl :: KempeDecl a b b -> Maybe (Warning b)
lintDecl Export{}             = Nothing
lintDecl TyDecl{}             = Nothing
lintDecl ExtFnDecl{}          = Nothing
lintDecl (FunDecl _ _ _ _ as) = lintAtoms as

lintAtoms :: [Atom b b] -> Maybe (Warning b)
lintAtoms []                       = Nothing
lintAtoms (a@(Dip l _):a'@Dip{}:_) = Just (DoubleDip l a a')
lintAtoms (_:as)                   = lintAtoms as
