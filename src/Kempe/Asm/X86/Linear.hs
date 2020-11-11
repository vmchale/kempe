-- | Linear scan register allocator
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            ) where

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
