module Kempe.AbsAsm ( size
                    , writeAtom
                    , Statement (..)
                    ) where

import           Data.Int  (Int64)
import           Data.Word (Word8)
import           Kempe.AST

data Label

data Temp

-- Stack pointer?

data Statement = MovTemp Temp Expression
               | StmExpression Expression
               | Seq Statement Statement
               | LabeledStm Label

data Expression = ConstInt Int64
                | ConstBool Word8
                | Named Label
                | Do Statement Expression
                | ExprIntBinOp IntBinOp Expression Expression
                | ExprIntRel RelBinOp Expression Expression
                | MemGet Expression
                | Jump Expression [Label] -- [Label] is all the possible targets; Expression is the address we're jumping to

data RelBinOp = IntEq
              | IntNeq
              | IntLt
              | IntGt

data IntBinOp = IntPlus
              | IntTimes
              | IntDiv
              | IntMinus
              | IntMod

writeAtom :: Atom MonoStackType -> [Statement]
writeAtom = undefined

size :: KempeTy a -> Int
size (TyBuiltin _ TyInt)  = 8 -- since we're only targeting x86_64 and aarch64 we have 64-bit 'Int's
size (TyBuiltin _ TyPtr)  = 8
size (TyBuiltin _ TyBool) = 1
size TyVar{}              = error "Internal error: type variables not allowed at this stage."
size (TyTuple _ tys)      = sum (fmap size tys)
