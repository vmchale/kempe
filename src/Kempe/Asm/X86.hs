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
import qualified Kempe.IR          as IR

data AbsReg = StackPointer
            | AllocReg !Int -- TODO: register by size

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

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

-- I'm kind of making up these instruction costs, I should look at the Agner
-- guides.
expCostAnn :: IR.Exp () -> IR.Exp Int
expCostAnn = cata a where
    a IR.StackPointerF{}           = IR.StackPointer 0
    a (IR.MemF _ e)                = IR.Mem (1 + IR.expCost e) e
    a (IR.ExprIntBinOpF _ op e e') = IR.ExprIntBinOp (1 + IR.expCost e + IR.expCost e') op e e' -- FIXME: per-op
    a (IR.ExprIntRelF _ op e e')   = IR.ExprIntRel (1 + IR.expCost e + IR.expCost e') op e e'

irToX86 :: [IR.Stmt ()] -> [X86 AbsReg]
irToX86 = concatMap (irEmit . irCosts)

irCosts :: IR.Stmt () -> IR.Stmt Int
irCosts (IR.Eff _ e)     = let e' = expCostAnn e in IR.Eff (IR.expCost e') e'
irCosts (IR.Jump _ l)    = IR.Jump 1 l
irCosts (IR.KCall _ l)   = IR.KCall 2 l
irCosts (IR.Push _ 8 e)  = let e' = expCostAnn e in IR.Push (1 + IR.expCost e') 8 e'
irCosts (IR.Labeled _ l) = IR.Labeled 0 l

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int -> [X86 AbsReg]
irEmit (IR.Jump _ l)  = [Jump l]

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
