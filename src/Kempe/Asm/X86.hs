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
                     , irToX86
                     , AbsReg (..)
                     ) where

import           Control.Recursion (cata)
import           Data.Int          (Int64)
import           Data.Word         (Word8)
import qualified Kempe.IR          as IR

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg8 i
toAbsReg (IR.Temp64 i)  = AllocReg64 i
toAbsReg IR.DataPointer = DataPointer

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CRet -- x0 on aarch64

-- parametric in @reg@ as we do register allocation in a separate phase
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
             | PushConst Int64
             | Jump IR.Label
             | Call IR.Label
             | Ret
             | MovRR reg reg
             | MovRC reg Int64 -- or Word64 ig
             | AddRC reg Int64
             | SubRC reg Int64
             | Label IR.Label
             | Je IR.Label
             | CmpRegConstU8 reg Word8

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

expCostAnn :: IR.Exp () -> IR.Exp Int
expCostAnn = cata a where
    a (IR.MemF _ e)                = IR.Mem (1 + IR.expCost e) e
    a (IR.ExprIntBinOpF _ op e e') = IR.ExprIntBinOp (1 + IR.expCost e + IR.expCost e') op e e' -- FIXME: per-op
    a (IR.ExprIntRelF _ op e e')   = IR.ExprIntRel (1 + IR.expCost e + IR.expCost e') op e e'

irToX86 :: [IR.Stmt ()] -> [X86 AbsReg]
irToX86 = concatMap (irEmit . irCosts)

irCosts :: IR.Stmt () -> IR.Stmt Int
irCosts (IR.Jump _ l)       = IR.Jump 1 l
irCosts (IR.KCall _ l)      = IR.KCall 2 l
irCosts (IR.Labeled _ l)    = IR.Labeled 0 l
irCosts (IR.CJump _ t l l') = IR.CJump 3 t l l'
irCosts (IR.MovTemp _ t e)  = let e' = expCostAnn e in IR.MovTemp (1 + IR.expCost e') t e'

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int -> [X86 AbsReg]
irEmit (IR.Jump _ l)       = [Jump l]
irEmit (IR.Labeled _ l)    = [Label l]
irEmit (IR.CJump _ t l l') = [CmpRegConstU8 (toAbsReg t) 0, Je l', Je l]

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
