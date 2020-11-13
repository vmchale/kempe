{-# LANGUAGE DataKinds #-}

module Kempe.Asm.X86.Dyn ( assemble
                         ) where

import           CodeGen.X86
import           Data.Foldable      (traverse_)
import           Kempe.Asm.X86.Type

assemble :: [X86 X86Reg ()] -> Code
assemble = traverse_ asmInstruction

asReg64 :: FromReg c => X86Reg -> c 'S64
asReg64 Rax = rax
asReg64 Rbx = rbx
asReg64 Rcx = rcx
asReg64 Rdx = rdx
asReg64 Rsp = rsp
asReg64 Rbp = rbp
asReg64 _   = error "Internal error: not a 64-bit register!"

asmInstruction :: X86 X86Reg () -> Code
asmInstruction Ret{}         = ret
asmInstruction (PushReg _ r) = push (asReg64 r)
