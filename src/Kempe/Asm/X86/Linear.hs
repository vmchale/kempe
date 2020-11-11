-- | Linear scan register allocator
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            , allocReg
                            ) where

import           Kempe.Asm.X86.Type

-- currently just has 64-bit and 8-bit registers
data X86Reg = Rax
            | Rbx
            | Rcx
            | Rdx
            | Rsp
            | Rbp
            | AH
            | AL
            | BH
            | BL
            | CH
            | CL
            | DH
            | DL

allocReg :: [X86 AbsReg Liveness] -> [X86 X86Reg ()]
allocReg = undefined
