module Kempe.AbsAsm ( size
                    , writeAtom
                    , Statement (..)
                    ) where

import           Data.Int  (Int64)
import           Data.Word (Word8)
import           Kempe.AST

data Label

data Temp

-- defined on aarch64 and x86_64
stackPointer :: Temp
stackPointer = undefined

-- FIXME push/pop not mov? in IR
data Statement = Push Temp
               | Pop Temp
               | MovTmp Expression
               | LabeledStm Label
               | Jump Expression [Label] -- [Label] is all the possible targets; Expression is the address we're jumping to

data Expression = ConstInt Int64
                | ConstantPtr Int64
                | ConstBool Word8
                | Named Label
                | Do Statement Expression
                | ExprIntBinOp IntBinOp Expression Expression
                | ExprIntRel RelBinOp Expression Expression

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
