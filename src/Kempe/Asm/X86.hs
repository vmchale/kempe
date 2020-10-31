module Kempe.Asm.X86 ( X86 (..)
                     , irToX86
                     , AbsReg
                     ) where

import           Control.Recursion (cata)
import           Data.Int          (Int64)
import           Kempe.IR

type AbsReg = Int

-- parametric in @reg@ so we can do register allocation later
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
             | PushConst Int64

-- how to represent a 'tiling'?? a 'tile' is an 'X86 AbsReg'?

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

-- I'm kind of making up these instruction costs, I should look at the Agner
-- guides.
expCostAnn :: Exp () -> Exp Int
expCostAnn = cata a where -- TODO: bench overhead from recursion schemes?
    a StackPointerF{} = StackPointer 0
    a (MemF _ e)      = Mem (1 + expCost e) e

irToX86 :: Stmt () -> [X86 AbsReg]
irToX86 = irEmit . irCosts

irCosts :: Stmt () -> Stmt Int
irCosts (Eff _ e)  = let e' = expCostAnn e in Eff (expCost e') e'
irCosts (Jump _ l) = Jump 1 l

-- does this need a monad for labels/intermediaries?
irEmit :: Stmt Int -> [X86 AbsReg]
irEmit = undefined
-- I wonder if I could use a hylo.?
