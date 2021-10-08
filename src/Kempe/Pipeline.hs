module Kempe.Pipeline ( irGen
                      , x86Parsed
                      , x86Alloc
                      , armParsed
                      , armAlloc
                      ) where

import           Control.Composition       ((.*))
import           Control.Exception         (throw)
import           Data.Bifunctor            (first)
import           Data.Typeable             (Typeable)
import           Kempe.AST
import           Kempe.AST.Size
import qualified Kempe.Asm.Arm.ControlFlow as Arm
import qualified Kempe.Asm.Arm.Linear      as Arm
import           Kempe.Asm.Arm.Opt
import           Kempe.Asm.Arm.Trans
import qualified Kempe.Asm.Arm.Type        as Arm
import           Kempe.Asm.Liveness
import qualified Kempe.Asm.X86.ControlFlow as X86
import qualified Kempe.Asm.X86.Linear      as X86
import           Kempe.Asm.X86.Trans
import qualified Kempe.Asm.X86.Type        as X86
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

armParsed :: Typeable a => Int -> Declarations a c b -> [Arm.Arm Arm.AbsReg ()]
armParsed i m = let (ir, u, env) = irGen i m in irToAarch64 env u ir

armAlloc :: Typeable a => Int -> Declarations a c b -> [Arm.Arm Arm.ArmReg ()]
armAlloc = optimizeArm . Arm.allocRegs . reconstruct . Arm.mkControlFlow .* armParsed

x86Parsed :: Typeable a => Int -> Declarations a c b -> [X86.X86 X86.AbsReg ()]
x86Parsed i m = let (ir, u, env) = irGen i m in irToX86 env u ir

x86Alloc :: Typeable a => Int -> Declarations a c b -> [X86.X86 X86.X86Reg ()]
x86Alloc = X86.allocRegs . reconstruct . X86.mkControlFlow .* x86Parsed
