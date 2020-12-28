module Kempe.Pipeline ( irGen
                      , x86Parsed
                      , x86Alloc
                      ) where

import           Control.Composition       ((.*))
import           Control.Exception         (throw)
import           Data.Bifunctor            (first)
import           Data.Typeable             (Typeable)
import           Kempe.AST
import           Kempe.AST.Size
import           Kempe.Asm.Liveness
import           Kempe.Asm.X86
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Type
import           Kempe.Check.Restrict
import           Kempe.IR
import           Kempe.IR.Opt
import           Kempe.IR.Type
import           Kempe.Shuttle

irGen :: Typeable a
      => Int -- ^ Thread uniques through
      -> Declarations a c b -> ([Stmt], WriteSt, SizeEnv)
irGen i m = adjEnv $ first optimize $ runTempM (writeModule env tAnnMod)
    where (tAnnMod, env) = either throw id $ monomorphize i mOk
          mOk = maybe m throw (restrictConstructors m)
          adjEnv (x, y) = (x, y, env)

x86Parsed :: Typeable a => Int -> Declarations a c b -> [X86 AbsReg ()]
x86Parsed i m = let (ir, u, env) = irGen i m in irToX86 env u ir

x86Alloc :: Typeable a => Int -> Declarations a c b -> [X86 X86Reg ()]
x86Alloc = allocRegs . reconstruct . mkControlFlow .* x86Parsed
