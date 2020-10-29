module Kempe.Asm.X86 ( X86 (..)
                     ) where

-- parametric in @reg@ so we can do register allocation later
data X86 reg = PushReg reg
             | PopReg reg
             | AddRR reg reg
             | SubRR reg reg
