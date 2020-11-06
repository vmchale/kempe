{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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

import           Control.DeepSeq     (NFData)
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable.Ext
import           Data.Int            (Int64)
import           Data.Word           (Word8)
import           GHC.Generics        (Generic)
import           Kempe.AST
import qualified Kempe.IR            as IR

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg8 i
toAbsReg (IR.Temp64 i)  = AllocReg64 i
toAbsReg IR.DataPointer = DataPointer

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CRet -- x0 on aarch64
            deriving (Generic, NFData)

type WriteM = State IR.WriteSt

nextLabels :: IR.WriteSt -> IR.WriteSt
nextLabels (IR.WriteSt ls ts) = IR.WriteSt (tail ls) ts

nextInt :: IR.WriteSt -> IR.WriteSt
nextInt (IR.WriteSt ls ts) = IR.WriteSt ls (tail ts)

getInt :: WriteM Int
getInt = gets (head . IR.temps) <* modify nextInt

getLabel :: WriteM IR.Label
getLabel = gets (head . IR.wlabels) <* modify nextLabels

allocReg64 :: WriteM AbsReg
allocReg64 = AllocReg64 <$> getInt

allocReg8 :: WriteM AbsReg
allocReg8 = AllocReg8 <$> getInt

runWriteM :: IR.WriteSt -> WriteM a -> a
runWriteM = flip evalState

data Addr reg = Reg reg
              | AddrRRPlus reg reg
              | AddrRCPlus reg Int64
              | AddrRCMinus reg Int64
              | AddrRRScale reg reg Int64
              deriving (Generic, NFData)

-- TODO: sanity-check pass to make sure no Reg8's are in e.g. MovRCBool

-- parametric in @reg@ as we do register allocation in a separate phase
data X86 reg = PushReg reg
             | PopReg reg
             | PushConst Int64
             | Jump IR.Label
             | Call IR.Label
             | Ret
             -- intel-ish syntax; destination first
             | MovRA reg (Addr reg)
             | MovAR (Addr reg) reg
             | MovABool (Addr reg) Word8
             | MovRR reg reg -- for convenience
             | MovRC reg Int64
             | MovRCBool reg Word8
             | AddRR reg reg
             | SubRR reg reg
             | MulRR reg reg
             | AddRC reg Int64
             | SubRC reg Int64
             | Label IR.Label
             | Je IR.Label
             | CmpAddrReg (Addr reg) reg
             | CmpRegReg reg reg -- for simplicity
             deriving (Generic, NFData)

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

irToX86 :: IR.WriteSt -> [IR.Stmt ()] -> [X86 AbsReg]
irToX86 w = runWriteM w . foldMapA (irEmit . irCosts)

-- TODO: match seq?
irCosts :: IR.Stmt () -> IR.Stmt Int
irCosts (IR.Jump _ l)                                                                                                                 = IR.Jump 1 l
irCosts (IR.KCall _ l)                                                                                                                = IR.KCall 2 l
irCosts IR.Ret{}                                                                                                                      = IR.Ret 1
irCosts (IR.Labeled _ l)                                                                                                              = IR.Labeled 0 l
irCosts (IR.CJump _ e@(IR.Mem (IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{})) l l')                                            = IR.CJump 3 e l l'
irCosts (IR.MovTemp _ r m@(IR.Mem IR.Reg{}))                                                                                          = IR.MovTemp 1 r m
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp IR.IntMinusIR IR.Reg{} IR.ConstInt{}))                                                     = IR.MovTemp 1 r e
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{}))                                                      = IR.MovTemp 1 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntBinOp IR.IntMinusIR IR.Reg{} IR.Reg{}))                                                  = IR.MovMem 2 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@IR.ConstInt{})                                                                                      = IR.MovMem 1 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntBinOp IR.IntTimesIR _ _))                                                                = IR.MovMem 3 r e
irCosts (IR.MovMem _ e1@(IR.ExprIntBinOp _ IR.Reg{} IR.ConstInt{}) e2@(IR.Mem (IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{}))) = IR.MovMem 2 e1 e2
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntRel _ IR.Reg{} IR.Reg{}))                                                                = IR.MovMem 2 r e
irCosts (IR.WrapKCall _ Cabi _ _ _)                                                                                                   = undefined

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int -> WriteM [X86 AbsReg]
irEmit (IR.Jump _ l) = pure [Jump l]
irEmit (IR.Labeled _ l) = pure [Label l]
irEmit (IR.KCall _ l) = pure [Call l]
irEmit IR.Ret{} = pure [Ret]
irEmit (IR.CJump _ (IR.Mem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r) (IR.ConstInt i))) l l') = do
    { r' <- allocReg8
    ; pure [MovRCBool r' 0, CmpAddrReg (AddrRCPlus (toAbsReg r) i) r', Je l, Jump l']
    }
irEmit (IR.MovTemp _ r (IR.Mem (IR.Reg r1))) = pure [MovRR (toAbsReg r) (toAbsReg r1)] -- TODO: use the same reg i?
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i))) = pure [MovRA (toAbsReg r) (AddrRCMinus (toAbsReg r1) i)]
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i))) = pure [MovRA (toAbsReg r) (AddrRCPlus (toAbsReg r1) i)]
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2))) = do -- this is a pain in the ass, maybe there is a better way to do this? -> pattern match on two sequenced instructions
    { r' <- allocReg64
    ; pure [ MovRR r' (toAbsReg r1), SubRR r' (toAbsReg r2), MovRR (toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg r) (IR.ConstInt i)) = pure [ MovRC (toAbsReg r) i ]
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR r' (toAbsReg r1), MulRR r' (toAbsReg r2), MovRR (toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.ExprIntBinOp _ (IR.Reg r0) (IR.ConstInt i)) (IR.Mem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA r' (AddrRCPlus (toAbsReg r1) j), MovAR (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntRel IR.IntEqIR (IR.Reg r1) (IR.Reg r2))) = do -- idk how to convert 4 bytes to one lmao -> need cmp instruction
    { l0 <- getLabel
    ; l1 <- getLabel
    ; pure [ CmpRegReg (toAbsReg r1) (toAbsReg r2), Je l0, Jump l1, Label l0, MovRCBool (toAbsReg r) 1, Label 1, MovRCBool (toAbsReg r) 0 ]
    }

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
