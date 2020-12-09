module Kempe.Pipeline ( irGen
                      , x86Parsed
                      , x86Alloc
                      ) where

import           Control.Composition       ((.*))
import           Control.Exception         (throw)
import           Data.Bifunctor            (first)
import           Kempe.AST
import           Kempe.Asm.X86
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Liveness
import           Kempe.Asm.X86.Type
import           Kempe.IR
import           Kempe.IR.Opt
import           Kempe.Shuttle

irGen :: Int -- ^ Thread uniques through
      -> Declarations a c b -> ([Stmt], WriteSt, SizeEnv)
irGen i m = adjEnv $ first optimize $ runTempM (writeModule env tAnnMod)
    where (tAnnMod, env) = either throw id $ monomorphize i m
          adjEnv (x, y) = (x, y, env)

x86Parsed :: Int -> Declarations a c b -> [X86 AbsReg ()]
x86Parsed i m = let (ir, u, env) = irGen i m in irToX86 env u ir

x86Alloc :: Int -> Declarations a c b -> [X86 X86Reg ()]
x86Alloc = allocRegs . reconstruct . mkControlFlow .* x86Parsed
