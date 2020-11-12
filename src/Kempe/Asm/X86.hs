-- | This module contains dynamic-programming optimum instruction selector.
--
-- It's kind of broken because
--
-- 1. I didn't actually look up the instruction costs in the Agner guides,
-- I just guessed.
--
-- 2. There aren't many possible tilings included.
--
-- Nathelees, it should be possible to amend this to a correct implementation.
module Kempe.Asm.X86 ( X86 (..)
                     , Addr (..)
                     , irToX86
                     , AbsReg (..)
                     , runWriteM
                     , WriteM
                     ) where

import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Foldable.Ext
import           Data.Int            (Int64)
import           Data.Monoid         (Sum (..))
import           Kempe.AST
import           Kempe.Asm.X86.Type
import qualified Kempe.IR            as IR

toAbsReg :: IR.Temp -> AbsReg
toAbsReg (IR.Temp8 i)   = AllocReg8 i
toAbsReg (IR.Temp64 i)  = AllocReg64 i
toAbsReg IR.DataPointer = DataPointer

type WriteM = State IR.WriteSt

nextLabels :: IR.WriteSt -> IR.WriteSt
nextLabels (IR.WriteSt ls ts) = IR.WriteSt (tail ls) ts

nextInt :: IR.WriteSt -> IR.WriteSt
nextInt (IR.WriteSt ls ts) = IR.WriteSt ls (tail ts)

getInt :: WriteM Int
getInt = gets (head . IR.temps) <* modify nextInt

getLabel :: WriteM IR.Label
getLabel = gets (head . IR.wlabels) <* modify nextLabels

allocReg64 :: WriteM AbsReg
allocReg64 = AllocReg64 <$> getInt

allocReg8 :: WriteM AbsReg
allocReg8 = AllocReg8 <$> getInt

runWriteM :: IR.WriteSt -> WriteM a -> a
runWriteM = flip evalState

-- first pass (bottom-up): annotate optimum tilings of subtrees w/ cost, use
-- that to annotate node with cost
-- second pass: write code

irToX86 :: IR.WriteSt -> [IR.Stmt ()] -> [X86 AbsReg ()]
irToX86 w = runWriteM w . foldMapA (irEmit . irCosts)

-- TODO: match multiple statements
-- this isn't even recursive lmao
-- TODO: something to eval general expressions
irCosts :: IR.Stmt () -> IR.Stmt Int64
irCosts (IR.Jump _ l)                                                                                                                 = IR.Jump 1 l
irCosts (IR.KCall _ l)                                                                                                                = IR.KCall 2 l
irCosts IR.Ret{}                                                                                                                      = IR.Ret 1
irCosts (IR.Labeled _ l)                                                                                                              = IR.Labeled 0 l
irCosts (IR.CJump _ e@(IR.Mem _ (IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{})) l l')                                            = IR.CJump 3 e l l'
irCosts (IR.MovTemp _ r m@(IR.Mem _ IR.Reg{}))                                                                                          = IR.MovTemp 1 r m
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp IR.IntMinusIR IR.Reg{} IR.ConstInt{}))                                                     = IR.MovTemp 1 r e
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{}))                                                      = IR.MovTemp 1 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntBinOp IR.IntMinusIR IR.Reg{} IR.Reg{}))                                                  = IR.MovMem 2 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@IR.ConstInt{})                                                                                      = IR.MovMem 1 r e
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntBinOp IR.IntTimesIR _ _))                                                                = IR.MovMem 3 r e
irCosts (IR.MovMem _ e1@(IR.ExprIntBinOp _ IR.Reg{} IR.ConstInt{}) e2@(IR.Mem _ (IR.ExprIntBinOp IR.IntPlusIR IR.Reg{} IR.ConstInt{}))) = IR.MovMem 2 e1 e2
irCosts (IR.MovMem _ r@IR.Reg{} e@(IR.ExprIntRel _ IR.Reg{} IR.Reg{}))                                                                = IR.MovMem 2 r e
irCosts (IR.WrapKCall _ Cabi (is, o) n l) | all (\i -> IR.size i `rem` 4 == 0) is = IR.WrapKCall (3 + sizeStack is `quot` 8) Cabi (is, o) n l

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int64 -> WriteM [X86 AbsReg ()]
irEmit (IR.Jump _ l) = pure [Jump () l]
irEmit (IR.Labeled _ l) = pure [Label () l]
irEmit (IR.KCall _ l) = pure [Call () l]
irEmit IR.Ret{} = pure [Ret ()]
irEmit (IR.CJump _ (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r) (IR.ConstInt i))) l l') = do
    { r' <- allocReg8
    ; pure [MovRCBool () r' 0, CmpAddrReg () (AddrRCPlus (toAbsReg r) i) r', Je () l, Jump () l']
    }
irEmit (IR.MovTemp _ r (IR.Mem _ (IR.Reg r1))) = pure [MovRA () (toAbsReg r) (Reg $ toAbsReg r1)] -- TODO: use the same reg i? TODO: sanity check reg/mem access size?
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.ConstInt i))) | r == r1 = pure [AddRC () (toAbsReg r) i]
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt i))) | r == r1 = pure [SubRC () (toAbsReg r) i]
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntBinOp IR.IntMinusIR (IR.Reg r1) (IR.Reg r2))) = do -- this is a pain in the ass, maybe there is a better way to do this? -> pattern match on two sequenced instructions
    { r' <- allocReg64
    ; pure [ MovRA () r' (Reg $ toAbsReg r1), SubRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg r) (IR.ConstInt i)) = pure [ MovAC () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntBinOp IR.IntTimesIR (IR.Reg r1) (IR.Reg r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), MulRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.ExprIntBinOp _ (IR.Reg r0) (IR.ConstInt i)) (IR.Mem 1 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem _ (IR.ExprIntBinOp _ (IR.Reg r0) (IR.ConstInt i)) (IR.Mem 8 (IR.ExprIntBinOp IR.IntPlusIR (IR.Reg r1) (IR.ConstInt j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg r) (IR.ExprIntRel IR.IntEqIR (IR.Reg r1) (IR.Reg r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Je () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Label () l1, MovABool () (Reg $ toAbsReg r) 0 ]
    }
irEmit (IR.WrapKCall _ Cabi (is, [o]) n l) | all (\i -> IR.size i `rem` 8 == 0) is && IR.size o == 8 = do
    { rC <- allocReg64 -- reg for offset counter
    ; let offs = zipWith const [1..] is
    ; let totalSize = sizeStack is + 8 -- for the output
    ; pure $ [BSLabel () n, MovRC () rC 0] ++ foldMap (\i -> [PopMem () (AddrRCPlus DataPointer (i * 8))]) offs ++ [AddAC () (Reg DataPointer) totalSize, Call () l, MovAR () (AddrRCMinus DataPointer 8) CRet, Ret ()] -- TODO: are the parameters backwards?
    -- copy last n bytes onto the system stack
    }

sizeStack :: [KempeTy a] -> Int64
sizeStack = getSum . foldMap (Sum . IR.size)

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
