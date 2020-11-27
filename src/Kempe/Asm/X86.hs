{-# LANGUAGE OverloadedStrings #-}

-- vaguely maximal munch
module Kempe.Asm.X86 ( X86 (..)
                     , Addr (..)
                     , irToX86
                     , AbsReg (..)
                     , runWriteM
                     , WriteM
                     ) where

import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Foldable.Ext
import           Data.List                  (scanl')
import           Data.Word                  (Word8)
import           Kempe.AST
import           Kempe.Asm.X86.Type
import qualified Kempe.IR                   as IR

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg8 i
toAbsReg (IR.Temp64 i)  = AllocReg64 i
toAbsReg IR.DataPointer = DataPointer

type WriteM = State IR.WriteSt

irToX86 :: IR.WriteSt -> [IR.Stmt] -> [X86 AbsReg ()]
irToX86 w = runWriteM w . foldMapA irEmit

nextLabels :: IR.WriteSt -> IR.WriteSt
nextLabels (IR.WriteSt ls ts) = IR.WriteSt (tail ls) ts

nextInt :: IR.WriteSt -> IR.WriteSt
nextInt (IR.WriteSt ls ts) = IR.WriteSt ls (tail ts)

getInt :: WriteM Int
getInt = gets (head . IR.temps) <* modify nextInt

getLabel :: WriteM IR.Label
getLabel = gets (head . IR.wlabels) <* modify nextLabels

allocTemp64 :: WriteM IR.Temp
allocTemp64 = IR.Temp64 <$> getInt

allocTemp8 :: WriteM IR.Temp
allocTemp8 = IR.Temp8 <$> getInt

allocReg64 :: WriteM AbsReg
allocReg64 = AllocReg64 <$> getInt

allocReg8 :: WriteM AbsReg
allocReg8 = AllocReg8 <$> getInt

runWriteM :: IR.WriteSt -> WriteM a -> a
runWriteM = flip evalState

irEmit :: IR.Stmt -> WriteM [X86 AbsReg ()]
irEmit (IR.Jump l) = pure [Jump () l]
irEmit (IR.Labeled l) = pure [Label () l]
irEmit (IR.KCall l) = pure [Call () l]
irEmit IR.Ret = pure [Ret ()]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), SubRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstInt i)) =
    pure [ MovAC () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstBool b)) =
    pure [ MovABool () (Reg $ toAbsReg r) (toByte b) ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstTag b)) =
    pure [ MovACTag () (Reg $ toAbsReg r) b ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), ImulRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntEqIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Je () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntLtIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jl () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
-- For 128-bit returns we'd have to use rax and rdx
irEmit (IR.WrapKCall Cabi (is, [o]) n l) | all (\i -> size i <= 8) is && size o <= 8 && length is <= 6 = do
    { let offs = scanl' (+) 0 (fmap size is)
    ; let totalSize = sizeStack is
    ; let argRegs = [CArg1, CArg2, CArg3, CArg4, CArg5, CArg6]
    ; pure $ [BSLabel () n, MovRL () DataPointer "kempe_data"] ++ save ++ zipWith (\r i-> MovAR () (AddrRCPlus DataPointer i) r) argRegs offs ++ [AddRC () DataPointer totalSize, Call () l, MovRA () CRet (AddrRCMinus DataPointer (size o))] ++ restore ++ [Ret ()] -- TODO: bytes on the stack eh
    }
irEmit (IR.WrapKCall Kabi (_, _) n l) =
    pure [BSLabel () n, Call () l, Ret ()]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstInt8 i)) =
    pure [ MovACi8 () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstWord w)) = do
    { r' <- allocReg64
    ; pure [ MovRWord () r' w, MovAR () (Reg $ toAbsReg r) r' ]
    -- see: https://github.com/cirosantilli/x86-assembly-cheat/blob/master/x86-64/movabs.asm
    -- this is a limitation of nasm that must be worked around
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntXorIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), XorRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), AddRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.WordShiftRIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () r' (toAbsReg r1), ShiftRRR () r' ShiftExponent, MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.WordShiftLIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () r' (toAbsReg r1), ShiftLRR () r' ShiftExponent, MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntModIR (IR.Reg r1) (IR.Reg r2))) =
    -- QuotRes is rax, so move r1 to rax first
    pure [ MovRR () QuotRes (toAbsReg r1), Cqo (), IdivR () (toAbsReg r2), MovAR () (Reg $ toAbsReg r) RemRes ]
irEmit (IR.MovTemp r e) = evalE e r
-- give up; use recursive formulation
irEmit (IR.MovMem e 8 e') = do
    { r <- allocTemp64
    ; r' <- allocTemp64
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [MovAR () (Reg $ toAbsReg r) (toAbsReg r')])
    }
irEmit (IR.MovMem e 1 e') = do
    { r <- allocTemp8
    ; r' <- allocTemp8
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [MovAR () (Reg $ toAbsReg r) (toAbsReg r')])
    }
irEmit (IR.CJump (IR.Mem 1 (IR.Reg r)) l l') =
    pure [CmpAddrBool () (Reg (toAbsReg r)) 1, Je () l, Jump () l']
irEmit (IR.CJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r) (IR.ConstInt i))) l l') =
    pure [CmpAddrBool () (AddrRCMinus (toAbsReg r) i) 1, Je () l, Jump () l']
irEmit (IR.CJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r) (IR.ConstInt i))) l l') =
    pure [CmpAddrBool () (AddrRCPlus (toAbsReg r) i) 1, Je () l, Jump () l']
irEmit (IR.CJump e l l') = do
    { r <- allocTemp8
    ; bEval <- evalE e r
    ; pure (bEval ++ [CmpRegBool () (toAbsReg r) 1, Je () l, Jump () l'])
    }

save :: [X86 AbsReg ()]
save = PushReg () <$> [CalleeSave1, CalleeSave2, CalleeSave3, CalleeSave4, CalleeSave5, CalleeSave6]

restore :: [X86 AbsReg ()]
restore = PopReg () <$> [CalleeSave6, CalleeSave5, CalleeSave4, CalleeSave3, CalleeSave2, CalleeSave1]

-- rbx, rbp, r12-r15 callee-saved (non-volatile)
-- rest caller-saved (volatile)

-- | Code to evaluate and put some expression in a chosen 'Temp'
--
-- This more or less conforms to maximal munch.
evalE :: IR.Exp -> IR.Temp -> WriteM [X86 AbsReg ()]
evalE (IR.ConstInt i) (IR.Temp64 t)                                 = pure [MovRC () (AllocReg64 t) i]
evalE (IR.ConstBool b) (IR.Temp8 t)                                 = pure [MovRCBool () (AllocReg8 t) (toByte b)]
evalE (IR.ConstInt8 i) (IR.Temp8 t)                                 = pure [MovRCi8 () (AllocReg8 t) i]
evalE (IR.ConstWord w) (IR.Temp64 t)                                = pure [MovRWord () (AllocReg64 t) w]
evalE (IR.Reg (IR.Temp64 t)) (IR.Temp64 t')                         = pure [MovRR () (AllocReg64 t) (AllocReg64 t')]
evalE (IR.Reg (IR.Temp8 t)) (IR.Temp8 t')                           = pure [MovRR () (AllocReg8 t) (AllocReg8 t')]
evalE (IR.Mem _ (IR.Reg r1)) r                                      = pure [MovRA () (toAbsReg r) (Reg $ toAbsReg r1) ] -- TODO: sanity check reg/mem access size?
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r | r == r1 = pure [SubRC () (toAbsReg r) i]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r  | r == r1 = pure [AddRC () (toAbsReg r) i]
-- TODO: ShiftExponent and QuotRes and RemRes

toByte :: Bool -> Word8
toByte False = 0
toByte True  = 1

-- I wonder if I could use a hylo.?
