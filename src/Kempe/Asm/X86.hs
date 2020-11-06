-- | This module contains dynamic-programming optimum instruction selector.
--
-- It's kind of broken because
--
-- 1. I didn't actually look up the instruction costs in the Agner guides,
-- I just guessed.
--
-- 2. There aren't many possible tilings included.
--
-- Nathelees, it should be possible to amend this to a correct implementation.
module Kempe.Asm.X86 ( X86 (..)
                     , Addr (..)
                     , irToX86
                     , AbsReg (..)
                     , runWriteM
                     , WriteM
                     ) where

import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable.Ext
import           Data.Int            (Int64)
import           Data.Word           (Word8)
import qualified Kempe.IR            as IR

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg8 i
toAbsReg (IR.Temp64 i)  = AllocReg64 i
toAbsReg IR.DataPointer = DataPointer

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CRet -- x0 on aarch64

newtype WriteSt = WriteSt { temps :: [Int] }

type WriteM = State WriteSt

nextInt :: WriteSt -> WriteSt
nextInt (WriteSt is) = WriteSt (tail is)

getInt :: WriteM Int
getInt = gets (head . temps) <* modify nextInt

allocReg64 :: WriteM AbsReg
allocReg64 = AllocReg64 <$> getInt

allocReg8 :: WriteM AbsReg
allocReg8 = AllocReg8 <$> getInt

runWriteM :: Int -> WriteM a -> a
runWriteM u = flip evalState (WriteSt [u..])

data Addr reg = Reg reg
              | AddrRRPlus reg reg
              | AddrRCPlus reg Int64
              | AddrRCMinus reg Int64
              | AddrRRScale reg reg Int64

-- parametric in @reg@ as we do register allocation in a separate phase
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
             | PushConst Int64
             | Jump IR.Label
             | Call IR.Label
             | Ret
             | MovRA reg (Addr reg)
             | MovAR (Addr reg) reg
             | MovRCBool reg Word8
             | AddRC reg Int64
             | SubRC reg Int64
             | Label IR.Label
             | Je IR.Label
             | CmpAddrReg (Addr reg) reg

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

irToX86 :: Int -> [IR.Stmt ()] -> [X86 AbsReg]
irToX86 u = runWriteM u . foldMapA (irEmit . irCosts)

-- TODO: match seq?
irCosts :: IR.Stmt () -> IR.Stmt Int
irCosts (IR.Jump _ l)                                                                      = IR.Jump 1 l
irCosts (IR.KCall _ l)                                                                     = IR.KCall 2 l
irCosts IR.Ret{}                                                                           = IR.Ret 1
irCosts (IR.Labeled _ l)                                                                   = IR.Labeled 0 l
irCosts (IR.CJump _ e@(IR.Mem (IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{})) l l') = IR.CJump 3 e l l'
irCosts (IR.MovTemp _ r m@(IR.Mem IR.Reg{}))                                               = IR.MovTemp 1 r m

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int -> WriteM [X86 AbsReg]
irEmit (IR.Jump _ l) = pure [Jump l]
irEmit (IR.Labeled _ l) = pure [Label l]
irEmit (IR.KCall _ l) = pure [Call l]
irEmit IR.Ret{} = pure [Ret]
irEmit (IR.CJump _ (IR.Mem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg IR.DataPointer) (IR.ConstInt i))) l l') = do
    { r <- allocReg8
    ; pure [MovRCBool r 0, CmpAddrReg (AddrRCPlus DataPointer i) r, Je l, Jump l' ]
    }

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
