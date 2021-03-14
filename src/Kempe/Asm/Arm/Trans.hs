{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Trans ( irToAarch64
                           ) where

import           Data.Foldable.Ext  (foldMapA)
import           Data.List          (scanl')
import           Kempe.AST.Size
import           Kempe.Asm.Arm.Type
import           Kempe.IR.Monad
import qualified Kempe.IR.Type      as IR
import           Prettyprinter      (pretty)

irToAarch64 :: SizeEnv -> IR.WriteSt -> [IR.Stmt] -> [Arm AbsReg ()]
irToAarch64 env w = runWriteM w . foldMapA (irEmit env)

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg i
toAbsReg (IR.Temp64 i)  = AllocReg i
toAbsReg IR.DataPointer = DataPointer

allocReg :: WriteM AbsReg
allocReg = AllocReg <$> getInt

-- popFrame :: [Arm AbsReg ()]
-- popFrame =

-- example function call (arm) https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/15_AssemblyFunctions.pdf

irEmit :: SizeEnv -> IR.Stmt -> WriteM [Arm AbsReg ()]
irEmit _ (IR.Jump l)                    = pure [Branch () l]
irEmit _ IR.Ret                         = pure [Ret ()]
irEmit _ (IR.KCall l)                   = pure [BranchLink () l]
irEmit _ (IR.Labeled l)                 = pure [Label () l]
irEmit _ (IR.WrapKCall Kabi (_, _) n l) = pure [BSLabel () n, BranchLink () l, Ret ()]
irEmit env (IR.WrapKCall Cabi (is, [o]) n l) | all (\i -> size' env i <= 8) is && size' env o <= 8 && length is <= 8 = do
    { let offs = scanl' (+) 0 (fmap (size' env) is)
    ; let totalSize = sizeStack env is
    ; let argRegs = [CArg0, CArg1, CArg2, CArg3, CArg4, CArg5, CArg6, CArg7]
    ; pure $ [BSLabel () n, LoadLabel () DataPointer "kempe_data", GnuMacro () "calleesave"] ++ undefined
    }
irEmit _ (IR.MovMem (IR.Reg r) 8 e) = do
    { r' <- allocTemp64
    ; put <- evalE e r'
    ; pure $ put ++ [Store () (toAbsReg r') (Reg $ toAbsReg r)]
    }
irEmit _ (IR.MovMem e 8 e') = do
    { r <- allocTemp64
    ; r' <- allocTemp64
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [Store () (toAbsReg r') (Reg $ toAbsReg r)])
    }
irEmit _ (IR.MovTemp r e) = evalE e r
irEmit _ (IR.MovMem e 1 e') = do -- TODO: IR.MovMem (Reg r) sz e' ...
    { r <- allocTemp64
    ; r' <- allocTemp64
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [StoreByte () (toAbsReg r') (Reg $ toAbsReg r)])
    }
irEmit _ (IR.CJump e l0 l1) = do
    { r <- allocTemp64
    ; eEval <- evalE e r
    ; pure $ eEval ++ [BranchZero () (toAbsReg r) l1, Branch () l0]
    }
irEmit _ e = error (show $ pretty e)

evalE :: IR.Exp -> IR.Temp -> WriteM [Arm AbsReg ()]
evalE (IR.ConstInt i) r                                            = pure [MovRC () (toAbsReg r) i]
evalE (IR.ConstWord w) r                                           = pure [MovRWord () (toAbsReg r) w]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2)) r     = pure [AddRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2)) r    = pure [SubRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2)) r    = pure [MulRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntDivIR (IR.Reg r1) (IR.Reg r2)) r      = pure [SignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.WordDivIR (IR.Reg r1) (IR.Reg r2)) r     = pure [UnsignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r = pure [AddRC () (toAbsReg r) (toAbsReg r1) i]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r = pure [SubRC () (toAbsReg r) (toAbsReg r1) i]
evalE (IR.Mem _ e) r = do
    { r' <- allocTemp64
    ; placeE <- evalE e r'
    ; pure $ placeE ++ [Load () (toAbsReg r) (Reg $ toAbsReg r')]
    }
evalE (IR.Reg r) r' = pure [MovRR () (toAbsReg r') (toAbsReg r)]
evalE (IR.ExprIntRel IR.IntEqIR e e') r = do -- TODO: e or e' a register (IR.ExprIntRel IR.IntEqIR r e' ...
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; eEval <- evalE e r0
    ; e'Eval <- evalE e' r1
    ; pure $ eEval ++ e'Eval ++ [CmpRR () (toAbsReg r0) (toAbsReg r1), CSet () (toAbsReg r) Eq]
    }
