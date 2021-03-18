{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Trans ( irToAarch64
                           ) where

import           Data.Foldable.Ext  (foldMapA)
import           Data.Int           (Int64)
import           Data.List          (scanl')
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

storeSize :: Int64 -> (reg -> Addr reg -> Arm reg ())
storeSize 1 = StoreByte ()
storeSize 8 = Store ()
storeSize _ = error "Load not supported or incoherent."

loadSize :: Int64 -> (reg -> Addr reg -> Arm reg ())
loadSize 1 = LoadByte ()
loadSize 8 = Load ()
loadSize _ = error "Load not supported or incoherent."

pushLink :: [Arm AbsReg ()]
pushLink = [SubRC () StackPtr StackPtr 16, Store () LinkReg (Reg StackPtr)]

popLink :: [Arm AbsReg ()]
popLink = [Load () LinkReg (Reg StackPtr), AddRC () StackPtr StackPtr 16]

irEmit :: SizeEnv -> IR.Stmt -> WriteM [Arm AbsReg ()]
irEmit _ (IR.Jump l)                    = pure [Branch () l]
irEmit _ IR.Ret                         = pure [Ret ()]
irEmit _ (IR.KCall l)                   = pure (pushLink ++ BranchLink () l : popLink) -- TODO: think more?
irEmit _ (IR.Labeled l)                 = pure [Label () l]
irEmit _ (IR.WrapKCall Kabi (_, _) n l) = pure $ [BSLabel () n] ++ pushLink ++ [BranchLink () l] ++ popLink ++ [Ret ()]
irEmit env (IR.WrapKCall Cabi (is, [o]) n l) | all (\i -> size' env i <= 8) is && size' env o <= 8 && length is <= 8 = do
    { let sizes = fmap (size' env) is
    ; let offs = scanl' (+) 0 sizes
    ; let totalSize = sizeStack env is
    ; let argRegs = [CArg0, CArg1, CArg2, CArg3, CArg4, CArg5, CArg6, CArg7]
    ; pure $ [BSLabel () n] ++ pushLink ++ [LoadLabel () DataPointer "kempe_data", GnuMacro () "calleesave"] ++ zipWith3 (\r sz i -> storeSize sz r (AddRCPlus DataPointer i)) argRegs sizes offs ++ [AddRC () DataPointer DataPointer totalSize, BranchLink () l, loadSize (size' env o) CArg0 (AddRCPlus DataPointer (negate $ size' env o)), GnuMacro () "calleerestore"] ++ popLink ++ [Ret ()]
    }
irEmit _ (IR.MovMem (IR.Reg r) 8 (IR.Reg r')) =
    pure [Store () (toAbsReg r') (Reg $ toAbsReg r)]
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
irEmit _ (IR.MovMem (IR.Reg r) 1 e) = do
    { r' <- allocTemp64
    ; put <- evalE e r'
    ; pure $ put ++ [StoreByte () (toAbsReg r') (Reg $ toAbsReg r)]
    }
irEmit _ (IR.MovMem e 1 e') = do
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
irEmit _ (IR.MJump e l) = do
    { r <- allocTemp64
    ; eEval <- evalE e r
    ; pure $ eEval ++ [BranchNonzero () (toAbsReg r) l] -- this is acceptable since in theory e is only 0 or 1
    }
-- example function call (arm) https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/15_AssemblyFunctions.pdf
--
-- try https://thinkingeek.com/2017/05/29/exploring-aarch64-assembler-chapter-8/

evalE :: IR.Exp -> IR.Temp -> WriteM [Arm AbsReg ()]
evalE (IR.ConstInt i) r                                                        = pure [MovRC () (toAbsReg r) i]
evalE (IR.ConstWord w) r                                                       = pure [MovRWord () (toAbsReg r) w]
evalE (IR.ConstTag b) r                                                        = pure [MovRC () (toAbsReg r) (fromIntegral b)]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2)) r                 = pure [AddRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2)) r                = pure [SubRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2)) r                = pure [MulRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntDivIR (IR.Reg r1) (IR.Reg r2)) r                  = pure [SignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.WordDivIR (IR.Reg r1) (IR.Reg r2)) r                 = pure [UnsignedDivRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.WordShiftRIR (IR.Reg r1) (IR.Reg r2)) r              = pure [LShiftRRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.WordShiftLIR (IR.Reg r1) (IR.Reg r2)) r              = pure [LShiftLRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntXorIR (IR.Reg r1) (IR.Reg r2)) r                  = pure [XorRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.BoolBinOp IR.BoolXor (IR.Reg r1) (IR.Reg r2)) r                      = pure [XorRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.BoolBinOp IR.BoolAnd (IR.Reg r1) (IR.Reg r2)) r                      = pure [AndRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.BoolBinOp IR.BoolOr (IR.Reg r1) (IR.Reg r2)) r                       = pure [OrRR () (toAbsReg r) (toAbsReg r1) (toAbsReg r2)]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r             = pure [AddRC () (toAbsReg r) (toAbsReg r1) i]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r            = pure [SubRC () (toAbsReg r) (toAbsReg r1) i]
evalE (IR.Mem 8 (IR.Reg r0)) r                                                 = pure [Load () (toAbsReg r) (Reg $ toAbsReg r0)]
evalE (IR.Mem 1 (IR.Reg r0)) r                                                 = pure [LoadByte () (toAbsReg r) (Reg $ toAbsReg r0)]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i))) r  = pure [Load () (toAbsReg r) (AddRCPlus (toAbsReg r0) i)]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i))) r = pure [Load () (toAbsReg r) (AddRCPlus (toAbsReg r0) (negate i))]
evalE (IR.Mem 8 e) r                                                           = do
    { r' <- allocTemp64
    ; placeE <- evalE e r'
    ; pure $ placeE ++ [Load () (toAbsReg r) (Reg $ toAbsReg r')]
    }
evalE (IR.Mem 1 e) r = do
    { r' <- allocTemp64
    ; placeE <- evalE e r'
    ; pure $ placeE ++ [LoadByte () (toAbsReg r) (Reg $ toAbsReg r')]
    }
evalE (IR.Reg r) r' = pure [MovRR () (toAbsReg r') (toAbsReg r)]
evalE (IR.ExprIntRel IR.IntLtIR (IR.Reg r1) (IR.Reg r2)) r =
    pure [CmpRR () (toAbsReg r1) (toAbsReg r2), CSet () (toAbsReg r) Lt]
evalE (IR.ExprIntRel IR.IntEqIR (IR.Reg r1) (IR.Reg r2)) r =
    pure [CmpRR () (toAbsReg r1) (toAbsReg r2), CSet () (toAbsReg r) Eq]
evalE (IR.ExprIntRel IR.IntEqIR e e') r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; eEval <- evalE e r0
    ; e'Eval <- evalE e' r1
    ; pure $ eEval ++ e'Eval ++ [CmpRR () (toAbsReg r0) (toAbsReg r1), CSet () (toAbsReg r) Eq]
    }
evalE (IR.EqByte e (IR.ConstTag b)) r = do
    { r0 <- allocTemp64
    ; eEval <- evalE e r0
    ; pure $ eEval ++ [CmpRC () (toAbsReg r0) (fromIntegral b), CSet () (toAbsReg r) Eq]
    }
evalE (IR.EqByte e e') r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; eEval <- evalE e r0
    ; e'Eval <- evalE e' r1
    ; pure $ eEval ++ e'Eval ++ [CmpRR () (toAbsReg r0) (toAbsReg r1), CSet () (toAbsReg r) Eq]
    }
evalE (IR.IntNegIR (IR.Reg r')) r = pure [Neg () (toAbsReg r) (toAbsReg r')]
evalE (IR.IntNegIR e) r = do
    { r' <- allocTemp64
    ; eEval <- evalE e r'
    ; pure $ eEval ++ [Neg () (toAbsReg r) (toAbsReg r')]
    }
evalE (IR.ExprIntBinOp IR.IntModIR (IR.Reg r1) (IR.Reg r2)) r = do
    { rTrash <- allocTemp64
    ; pure [ UnsignedDivRR () (toAbsReg rTrash) (toAbsReg r1) (toAbsReg r2), MulSubRRR () (toAbsReg r) (toAbsReg rTrash) (toAbsReg r2) (toAbsReg r1) ]
    }
evalE (IR.ConstBool b) r = pure [MovRC () (toAbsReg r) (toInt b)]

-- | Just use 64-bit integers here
toInt :: Bool -> Int64
toInt False = 0
toInt True  = 1
