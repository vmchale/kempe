module Kempe.Asm.X86 ( X86 (..)
                     , irEmit
                     , AbsReg
                     ) where

import           Data.Int (Int64)
import           Kempe.IR

data AbsReg

-- parametric in @reg@ so we can do register allocation later
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
             | PushConst Int64

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

irCosts :: Stmt () -> Stmt Int
irCosts = undefined

irEmit :: Stmt () -> [X86 AbsReg]
irEmit = undefined

-- pretty-printer for intel asm?
