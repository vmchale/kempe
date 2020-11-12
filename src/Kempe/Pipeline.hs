module Kempe.Pipeline ( irGen
                      , x86Parsed
                      , x86Alloc
                      ) where

import           Control.Composition       ((.*))
import           Control.Exception         (throw)
import           Kempe.AST
import           Kempe.Asm.X86
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Liveness
import           Kempe.IR
import           Kempe.Shuttle

irGen :: Int -- ^ Thread uniques through
      -> Module a b -> ([Stmt ()], WriteSt)
irGen i m = runTempM (writeModule tAnnMod)
    where tAnnMod = either throw id $ monomorphize i m

x86Parsed :: Int -> Module a b -> [X86 AbsReg ()]
x86Parsed i m = let (ir, u) = irGen i m in irToX86 u ir

x86Alloc :: Int -> Module a b -> [X86 X86Reg ()]
x86Alloc = allocRegs . reconstruct . mkControlFlow .* x86Parsed
