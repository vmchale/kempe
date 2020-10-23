module Kempe.IR ( size
                , stackPointer
                , writeAtom
                , Statement (..)
                , Expression (..)
                , RelBinOp (..)
                , IntBinOp (..)
                ) where

import           Data.Int  (Int64)
import           Data.Word (Word8)
import           Kempe.AST

data Label

data Temp

-- defined on aarch64 and x86_64
stackPointer :: Temp
stackPointer = undefined

-- TODO figure out dip
data Statement = Push Expression
               | Pop Expression Temp
               | Labeled Label
               -- -- | Seq Statement Statement
               | Jump Label
               | CJump Expression Label Label
               -- -- | CCall

data Expression = ConstInt Int64
                | ConstantPtr Int64
                | ConstBool Word8
                | Named Label
                | Reg Temp
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


-- need monad for fresh 'Temp's
writeAtom :: Atom MonoStackType -> [Statement]
writeAtom (IntLit _ i)  = [Push (ConstInt $ fromInteger i)]
writeAtom (BoolLit _ b) = [Push (ConstBool $ toByte b)]

toByte :: Bool -> Word8
toByte True  = 1
toByte False = 0

-- need env with size?
size :: KempeTy a -> Int
size (TyBuiltin _ TyInt)  = 8 -- since we're only targeting x86_64 and aarch64 we have 64-bit 'Int's
size (TyBuiltin _ TyPtr)  = 8
size (TyBuiltin _ TyBool) = 1
size TyVar{}              = error "Internal error: type variables not allowed at this stage."
size (TyTuple _ tys)      = sum (fmap size tys)
