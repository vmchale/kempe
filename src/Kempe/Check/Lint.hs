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
lintAtoms []                                                = Nothing
lintAtoms (a@(Dip l _):a'@Dip{}:_)                          = Just (DoubleDip l a a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ IntEq):_)     = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ IntNeq):_)    = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ And):_)       = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ Or):_)        = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ Xor):_)       = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ WordXor):_)   = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ IntTimes):_)  = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ IntPlus):_)   = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ WordPlus):_)  = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ WordTimes):_) = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin _ IntXor):_)    = Just (SwapBinary l a' a')
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin l' IntGt):_)    = Just (SwapBinary l a' (AtBuiltin l' IntLt))
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin l' IntGeq):_)   = Just (SwapBinary l a' (AtBuiltin l' IntLeq))
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin l' IntLt):_)    = Just (SwapBinary l a' (AtBuiltin l' IntGt))
lintAtoms ((AtBuiltin l Swap):a'@(AtBuiltin l' IntLeq):_)   = Just (SwapBinary l a' (AtBuiltin l' IntGeq))
lintAtoms (_:as)                                            = lintAtoms as
