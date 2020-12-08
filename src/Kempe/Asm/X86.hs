{-# LANGUAGE OverloadedStrings #-}

-- | Started out trying to implement maximal munch but ended with something
-- "flatter" that works with Kempe IR and my shitty register allocator.
module Kempe.Asm.X86 ( irToX86
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

irToX86 :: SizeEnv -> IR.WriteSt -> [IR.Stmt] -> [X86 AbsReg ()]
irToX86 env w = runWriteM w . foldMapA (irEmit env)

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

-- | This should handle 'MovMem's of divers sizes but for now it just does
-- 1 byte or 8 bytes at a time.
irEmit :: SizeEnv -> IR.Stmt -> WriteM [X86 AbsReg ()]
irEmit _ (IR.Jump l) = pure [Jump () l]
irEmit _ (IR.Labeled l) = pure [Label () l]
irEmit _ (IR.KCall l) = pure [Call () l]
irEmit _ IR.Ret = pure [Ret ()]
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), SubRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ConstInt i)) =
    pure [ MovAC () (Reg $ toAbsReg r) i ]
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ConstBool b)) =
    pure [ MovABool () (Reg $ toAbsReg r) (toByte b) ]
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ConstTag b)) =
    pure [ MovACTag () (Reg $ toAbsReg r) b ]
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), ImulRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCMinus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCMinus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r0) (IR.ConstInt i)) _ (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntEqIR (IR.Reg r1) (IR.Reg r2))) = do -- TODO: int eq more general (Reg r1) could be e1 &c.
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Je () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntLtIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jl () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntGtIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jg () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntGeqIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jge () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntNeqIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jne () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntRel IR.IntLeqIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; l2 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Jle () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Jump () l2, Label () l1, MovABool () (Reg $ toAbsReg r) 0, Label () l2 ]
    }
irEmit _ (IR.MovTemp r1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r2) (IR.ConstInt i))) | r1 == r2 = do
    pure [ SubRC () (toAbsReg r1) i ]
irEmit _ (IR.MovTemp r1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r2) (IR.ConstInt i))) | r1 == r2 = do
    pure [ AddRC () (toAbsReg r1) i ]
irEmit env (IR.CCall (is, []) b) | all (\i -> size' env i <= 8) is && length is <= 6 =
    pure [NasmMacro0 () "callersave", CallBS () b, NasmMacro0 () "callerrestore"]
irEmit env (IR.CCall (is, [o]) b) | all (\i -> size' env i <= 8) is && size' env o <= 8 && length is <= 6 =
    pure [NasmMacro0 () "callersave", CallBS () b, NasmMacro0 () "callerrestore"]
-- For 128-bit returns we'd have to use rax and rdx
irEmit env (IR.WrapKCall Cabi (is, [o]) n l) | all (\i -> size' env i <= 8) is && size' env o <= 8 && length is <= 6 = do
    { let offs = scanl' (+) 0 (fmap (size' env) is)
    ; let totalSize = sizeStack' env is
    ; let argRegs = [CArg1, CArg2, CArg3, CArg4, CArg5, CArg6]
    ; pure $ [BSLabel () n, MovRL () DataPointer "kempe_data", NasmMacro0 () "calleesave"] ++ zipWith (\r i-> MovAR () (AddrRCPlus DataPointer i) r) argRegs offs ++ [AddRC () DataPointer totalSize, Call () l, MovRA () CRet (AddrRCMinus DataPointer (size' env o)), NasmMacro0 () "calleerestore", Ret ()] -- TODO: bytes on the stack eh
    }
irEmit _ (IR.WrapKCall Kabi (_, _) n l) =
    pure [BSLabel () n, Call () l, Ret ()]
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ConstInt8 i)) =
    pure [ MovACi8 () (Reg $ toAbsReg r) i ]
    -- see: https://github.com/cirosantilli/x86-assembly-cheat/blob/master/x86-64/movabs.asm for why we don't do this ^ for words
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntXorIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), XorRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), AddRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.WordShiftRIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () r' (toAbsReg r1), ShiftRRR () r' ShiftExponent, MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.WordShiftLIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () r' (toAbsReg r1), ShiftLRR () r' ShiftExponent, MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit _ (IR.MovMem (IR.Reg r) _ (IR.ExprIntBinOp IR.IntModIR (IR.Reg r1) (IR.Reg r2))) =
    -- QuotRes is rax, so move r1 to rax first
    pure [ MovRR () QuotRes (toAbsReg r1), Cqo (), IdivR () (toAbsReg r2), MovAR () (Reg $ toAbsReg r) RemRes ]
irEmit _ (IR.MovTemp r e) = evalE e r
irEmit _ (IR.MovMem (IR.Reg r) 1 e) = do
    { r' <- allocTemp8
    ; put <- evalE e r'
    ; pure $ put ++ [MovAR () (Reg $ toAbsReg r) (toAbsReg r')]
    }
irEmit _ (IR.MovMem e 8 e') = do
    { r <- allocTemp64
    ; r' <- allocTemp64
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [MovAR () (Reg $ toAbsReg r) (toAbsReg r')])
    }
irEmit _ (IR.MovMem e 1 e') = do
    { r <- allocTemp64
    ; r' <- allocTemp8
    ; eEval <- evalE e r
    ; e'Eval <- evalE e' r'
    ; pure (eEval ++ e'Eval ++ [MovAR () (Reg $ toAbsReg r) (toAbsReg r')])
    }
irEmit _ (IR.CJump (IR.Mem 1 (IR.Reg r)) l l') =
    pure [CmpAddrBool () (Reg (toAbsReg r)) 1, Je () l, Jump () l']
irEmit _ (IR.MJump (IR.Mem 1 (IR.Reg r)) l) =
    pure [CmpAddrBool () (Reg (toAbsReg r)) 1, Je () l]
irEmit _ (IR.CJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r) (IR.ConstInt i))) l l') =
    pure [CmpAddrBool () (AddrRCMinus (toAbsReg r) i) 1, Je () l, Jump () l']
irEmit _ (IR.MJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r) (IR.ConstInt i))) l) =
    pure [CmpAddrBool () (AddrRCMinus (toAbsReg r) i) 1, Je () l]
irEmit _ (IR.CJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r) (IR.ConstInt i))) l l') =
    pure [CmpAddrBool () (AddrRCPlus (toAbsReg r) i) 1, Je () l, Jump () l']
irEmit _ (IR.MJump (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r) (IR.ConstInt i))) l) =
    pure [CmpAddrBool () (AddrRCPlus (toAbsReg r) i) 1, Je () l]
irEmit _ (IR.CJump e l l') = do
    { r <- allocTemp8
    ; bEval <- evalE e r
    ; pure (bEval ++ [CmpRegBool () (toAbsReg r) 1, Je () l, Jump () l'])
    }
irEmit _ (IR.MJump (IR.EqByte (IR.Mem 1 (IR.Reg r)) (IR.ConstTag b)) l) =
    pure [CmpAddrBool () (Reg $ toAbsReg r) b, Je () l]
irEmit _ (IR.MJump (IR.EqByte e0 e1) l) = do
    { r0 <- allocTemp8
    ; r1 <- allocTemp8
    ; placeE0 <- evalE e0 r0
    ; placeE1 <- evalE e1 r1
    ; pure $ placeE0 ++ placeE1 ++ [CmpRegReg () (toAbsReg r0) (toAbsReg r1), Je () l]
    }
irEmit _ (IR.MJump e l) = do
    { r <- allocTemp8
    ; bEval <- evalE e r
    ; pure (bEval ++ [CmpRegBool () (toAbsReg r) 1, Je () l])
    }

-- | Code to evaluate and put some expression in a chosen 'Temp'
--
-- This more or less conforms to maximal munch.
evalE :: IR.Exp -> IR.Temp -> WriteM [X86 AbsReg ()]
evalE (IR.ConstInt i) r                                             = pure [MovRC () (toAbsReg r) i]
evalE (IR.ConstBool b) r                                            = pure [MovRCBool () (toAbsReg r) (toByte b)]
evalE (IR.ConstInt8 i) r                                            = pure [MovRCi8 () (toAbsReg r) i]
evalE (IR.ConstWord w) r                                            = pure [MovRWord () (toAbsReg r) w]
evalE (IR.ConstTag b) r                                             = pure [MovRCTag () (toAbsReg r) b]
evalE (IR.Reg r') r                                                 = pure [MovRR () (toAbsReg r) (toAbsReg r')]
evalE (IR.Mem _ (IR.Reg r1)) r                                      = pure [MovRA () (toAbsReg r) (Reg $ toAbsReg r1) ] -- TODO: sanity check reg/mem access size?
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r | r == r1 = pure [SubRC () (toAbsReg r) i]
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i)) r  | r == r1 = pure [AddRC () (toAbsReg r) i]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt 0)) r = pure [MovRR () (toAbsReg r) (toAbsReg r1)]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i))) r =
    pure [ MovRA () (toAbsReg r) (AddrRCMinus (toAbsReg r1) i) ]
evalE (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i))) r =
    pure [ MovRA () (toAbsReg r) (AddrRCPlus (toAbsReg r1) i) ]
evalE (IR.Mem _ e) r = do -- don't need to check size b/c we're storing in r
    { r' <- allocTemp64
    ; placeE <- evalE e r'
    ; pure $ placeE ++ [MovRA () (toAbsReg r) (Reg $ toAbsReg r')]
    }
evalE (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.Reg r2)) r =
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), AddRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), ImulRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntXorIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), XorRR () (toAbsReg r) (toAbsReg r2) ]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), SubRC () (toAbsReg r) i ]
evalE (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2)) r = do
    pure [ MovRR () (toAbsReg r) (toAbsReg r1), SubRR () (toAbsReg r) (toAbsReg r2) ]
-- FIXME: because my linear register allocator is shitty, I can't keep
-- registers across jumps... so evaluating = or < as an expression in
-- general is hard ?
evalE (IR.ExprIntBinOp IR.WordShiftLIR (IR.Reg r1) (IR.Reg r2)) r =
    pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () (toAbsReg r) (toAbsReg r1), ShiftLRR () (toAbsReg r) ShiftExponent ]
evalE (IR.ExprIntBinOp IR.WordShiftRIR (IR.Reg r1) (IR.Reg r2)) r = -- FIXME: maximal munch use evalE recursively
    pure [ MovRR () ShiftExponent (toAbsReg r2), MovRR () (toAbsReg r) (toAbsReg r1), ShiftRRR () (toAbsReg r) ShiftExponent]
evalE (IR.ExprIntBinOp IR.WordShiftLIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp8
    ; placeE0 <- evalE e0 r0
    ; placeE1 <- evalE e1 r1
    ; pure $ placeE0 ++ placeE1 ++ [ MovRR () ShiftExponent (toAbsReg r0), MovRR () (toAbsReg r) (toAbsReg r1), ShiftLRR () (toAbsReg r) ShiftExponent ]
    }
evalE (IR.ExprIntBinOp IR.WordShiftRIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp8
    ; placeE0 <- evalE e0 r0
    ; placeE1 <- evalE e1 r1
    ; pure $ placeE0 ++ placeE1 ++ [ MovRR () ShiftExponent (toAbsReg r0), MovRR () (toAbsReg r) (toAbsReg r1), ShiftRRR () (toAbsReg r) ShiftExponent ]
    }
evalE (IR.ExprIntBinOp IR.IntModIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    -- QuotRes is rax, so move r1 to rax first
    ; pure $ placeE ++ placeE' ++ [ MovRR () QuotRes (toAbsReg r0), Cqo (), IdivR () (toAbsReg r1), MovRR () (toAbsReg r) RemRes ]
    }
evalE (IR.ExprIntBinOp IR.IntDivIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () QuotRes (toAbsReg r0), Cqo (), IdivR () (toAbsReg r1), MovRR () (toAbsReg r) QuotRes ]
    }
evalE (IR.ExprIntBinOp IR.WordDivIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () QuotRes (toAbsReg r0), XorRR () RemRes RemRes, DivR () (toAbsReg r1), MovRR () (toAbsReg r) QuotRes ]
    }
evalE (IR.ExprIntBinOp IR.WordModIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () QuotRes (toAbsReg r0), XorRR () RemRes RemRes, DivR () (toAbsReg r1), MovRR () (toAbsReg r) RemRes ]
    }
evalE (IR.BoolBinOp IR.BoolAnd e0 e1) r = do
    { r0 <- allocTemp8
    ; r1 <- allocTemp8
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), AndRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.BoolBinOp IR.BoolOr e0 e1) r = do
    { r0 <- allocTemp8
    ; r1 <- allocTemp8
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), OrRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.BoolBinOp IR.BoolXor e0 e1) r = do
    { r0 <- allocTemp8
    ; r1 <- allocTemp8
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), XorRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.ExprIntBinOp IR.IntMinusIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), SubRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.ExprIntBinOp IR.IntPlusIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), AddRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.ExprIntBinOp IR.IntTimesIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), ImulRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.ExprIntBinOp IR.IntXorIR e0 e1) r = do
    { r0 <- allocTemp64
    ; r1 <- allocTemp64
    ; placeE <- evalE e0 r0
    ; placeE' <- evalE e1 r1
    ; pure $ placeE ++ placeE' ++ [ MovRR () (toAbsReg r) (toAbsReg r0), XorRR () (toAbsReg r) (toAbsReg r1) ]
    }
evalE (IR.PopcountIR e0) r = do
    { r' <- allocTemp64
    ; placeE <- evalE e0 r'
    ; pure $ placeE ++ [ PopcountRR () (toAbsReg r) (toAbsReg r') ]
    }
evalE (IR.IntNegIR e) r = do
    { placeE <- evalE e r
    ; pure $ placeE ++ [ NegR () (toAbsReg r) ]
    }

toByte :: Bool -> Word8
toByte False = 0
toByte True  = 1

-- I wonder if I could use a hylo.?
