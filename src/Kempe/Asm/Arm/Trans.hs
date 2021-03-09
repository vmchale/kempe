module Kempe.Asm.Arm.Trans ( irToAarch64
                           ) where

import           Data.Foldable.Ext  (foldMapA)
import           Kempe.AST.Size
import           Kempe.Asm.Arm.Type
import           Kempe.IR.Monad
import qualified Kempe.IR.Type      as IR

irToAarch64 :: SizeEnv -> IR.WriteSt -> [IR.Stmt] -> [Arm AbsReg ()]
irToAarch64 env w = runWriteM w . foldMapA (irEmit env)

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg i
toAbsReg (IR.Temp64 i)  = AllocReg i
toAbsReg IR.DataPointer = DataPointer

allocReg :: WriteM AbsReg
allocReg = AllocReg <$> getInt

-- example function call (arm) https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/15_AssemblyFunctions.pdf

irEmit :: SizeEnv -> IR.Stmt -> WriteM [Arm AbsReg ()]
irEmit _ (IR.Jump l)                    = pure [Branch () l]
irEmit _ IR.Ret                         = pure [Ret ()]
irEmit _ (IR.KCall l)                   = pure [BranchLink () l]
irEmit _ (IR.Labeled l)                 = pure [Label () l]
irEmit _ (IR.WrapKCall Kabi (_, _) n l) = pure [BSLabel () n, BranchLink () l, Ret ()]

evalE :: IR.Exp -> IR.Temp -> WriteM [Arm AbsReg ()]
evalE (IR.ConstInt i) r                                            = pure [MovRC () (toAbsReg r) i]
evalE (IR.ConstWord w) r                                           = pure [MovRWord () (toAbsReg r) w]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2)) r     = pure [AddRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2)) r    = pure [SubRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2)) r    = pure [MulRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntDivIR (IR.Reg r1) (IR.Reg r2)) r      = pure [SignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.WordDivIR (IR.Reg r1) (IR.Reg r2)) r     = pure [UnsignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r = pure [AddRC () (toAbsReg r) (toAbsReg r1) i]
