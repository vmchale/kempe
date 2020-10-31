module Kempe.Asm.X86 ( X86 (..)
                     , irToX86
                     , AbsReg
                     ) where

import           Control.Recursion (cata)
import           Data.Int          (Int64)
import qualified Kempe.IR          as IR

type AbsReg = Int

-- parametric in @reg@ so we can do register allocation later
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
             | PushConst Int64
             | Jump IR.Label
             | Call IR.Label

-- how to represent a 'tiling'?? a 'tile' is an 'X86 AbsReg'?

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

-- I'm kind of making up these instruction costs, I should look at the Agner
-- guides.
expCostAnn :: IR.Exp () -> IR.Exp Int
expCostAnn = cata a where
    a IR.StackPointerF{} = IR.StackPointer 0
    a (IR.MemF _ e)      = IR.Mem (1 + IR.expCost e) e

irToX86 :: IR.Stmt () -> [X86 AbsReg]
irToX86 = irEmit . irCosts

irCosts :: IR.Stmt () -> IR.Stmt Int
irCosts (IR.Eff _ e)   = let e' = expCostAnn e in IR.Eff (IR.expCost e') e'
irCosts (IR.Jump _ l)  = IR.Jump 1 l
irCosts (IR.KCall _ l) = IR.KCall 2 l

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int -> [X86 AbsReg]
irEmit (IR.Jump _ l)  = [Jump l]
irEmit (IR.KCall _ l) = [Call l]
-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
