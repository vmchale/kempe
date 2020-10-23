module Kempe.IR ( size
                , stackPointer
                , writeAtoms
                , Statement (..)
                , Expression (..)
                , RelBinOp (..)
                , IntBinOp (..)
                ) where

import           Control.Monad.State (State, gets, modify)
import           Data.Bifunctor      (first, second)
import           Data.Foldable       (fold)
import           Data.Int            (Int64)
import           Data.Word           (Word8)
import           Kempe.AST

data Label

data Temp

-- defined on aarch64 and x86_64
stackPointer :: Temp
stackPointer = undefined

type TempM = State ([Label], [Temp])

getTemp :: TempM Temp
getTemp = gets (head . snd) <* modify (second tail)

newLabel :: TempM Label
newLabel = gets (head . fst) <* modify (first tail)

-- TODO figure out dip
data Statement = Push Expression
               | Pop (KempeTy ()) Temp
               | Labeled Label
               -- -- | Seq Statement Statement
               | Jump Label
               | CJump Expression Label Label
               -- -- | CCall MonoStackType

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


writeAtoms :: [Atom MonoStackType] -> TempM [Statement]
writeAtoms = foldMapA writeAtom where
    foldMapA = (fmap fold .) . traverse

-- need monad for fresh 'Temp's
writeAtom :: Atom MonoStackType -> TempM [Statement]
writeAtom (IntLit _ i)  = pure [Push (ConstInt $ fromInteger i)]
writeAtom (BoolLit _ b) = pure [Push (ConstBool $ toByte b)]

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
