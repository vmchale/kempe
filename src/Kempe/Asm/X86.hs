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
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstInt i)) =
    pure [ MovAC () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstBool b)) =
    pure [ MovABool () (Reg $ toAbsReg r) (toByte b) ]
irEmit (IR.MovMem (IR.Reg r) _ (IR.ConstTag b)) =
    pure [ MovACTag () (Reg $ toAbsReg r) b ]
irEmit (IR.MovTemp r1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r2) (IR.ConstInt i))) | r1 == r2 = do
    pure [ SubRC () (toAbsReg r1) i ]
irEmit (IR.MovTemp r1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r2) (IR.ConstInt i))) | r1 == r2 = do
    pure [ AddRC () (toAbsReg r1) i ]
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
irEmit (IR.MovTemp r e) = evalE e r
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
evalE (IR.ConstInt i) r                                             = pure [MovRC () (toAbsReg r) i]
evalE (IR.ConstBool b) r                                            = pure [MovRCBool () (toAbsReg r) (toByte b)]
evalE (IR.ConstInt8 i) r                                            = pure [MovRCi8 () (toAbsReg r) i]
evalE (IR.ConstWord w) r                                            = pure [MovRWord () (toAbsReg r) w]
evalE (IR.Reg r') r                                                 = pure [MovRR () (toAbsReg r) (toAbsReg r')]
evalE (IR.Mem _ (IR.Reg r1)) r                                      = pure [MovRA () (toAbsReg r) (Reg $ toAbsReg r1) ] -- TODO: sanity check reg/mem access size?
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r | r == r1 = pure [SubRC () (toAbsReg r) i]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r  | r == r1 = pure [AddRC () (toAbsReg r) i]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt 0)) r = pure [MovRR () (toAbsReg r) (toAbsReg r1)]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i))) r =
    pure [ MovRA () (toAbsReg r) (AddrRCMinus (toAbsReg r1) i) ]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i))) r =
    pure [ MovRA () (toAbsReg r) (AddrRCPlus (toAbsReg r1) i) ]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2)) r =
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), AddRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), ImulRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntXorIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), XorRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), SubRC () (toAbsReg r) i ]
evalE (IR.ExprIntRel IR.IntEqIR e e') r = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e r0
    ; placeE' <- evalE e' r1
    ; pure $ placeE ++ placeE' ++ [CmpRegReg () (toAbsReg r0) (toAbsReg r1), Je () l0, Jump () l1, Label () l0, MovRCBool () (toAbsReg r) 1, Jump () l2, Label () l1, MovRCBool () (toAbsReg r) 0, Label () l2 ]
    }
evalE (IR.ExprIntRel IR.IntLtIR e e') r = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e r0
    ; placeE' <- evalE e' r1
    ; pure $ placeE ++ placeE' ++ [CmpRegReg () (toAbsReg r0) (toAbsReg r1), Jl () l0, Jump () l1, Label () l0, MovRCBool () (toAbsReg r) 1, Jump () l2, Label () l1, MovRCBool () (toAbsReg r) 0, Label () l2 ]
    }
evalE (IR.ExprIntBinOp IR.WordShiftLIR (IR.Reg r1) (IR.Reg r2)) r = -- FIXME: maximal munch use evalE recursively
    pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () (toAbsReg r) (toAbsReg r1), ShiftLRR () (toAbsReg r) ShiftExponent ]
evalE (IR.ExprIntBinOp IR.WordShiftRIR (IR.Reg r1) (IR.Reg r2)) r = -- FIXME: maximal munch use evalE recursively
    pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () (toAbsReg r) (toAbsReg r1), ShiftRRR () (toAbsReg r) ShiftExponent]
evalE (IR.ExprIntBinOp IR.IntModIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    -- QuotRes is rax, so move r1 to rax first
    ; pure $ placeE ++ placeE' ++ [ MovRR () QuotRes (toAbsReg r0), Cqo (), IdivR () (toAbsReg r1), MovRR () (toAbsReg r) RemRes ]
    }
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), SubRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntMinusIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r0) (toAbsReg r), SubRR () (toAbsReg r) (toAbsReg r1) ]
    }

toByte :: Bool -> Word8
toByte False = 0
toByte True  = 1

-- I wonder if I could use a hylo.?
