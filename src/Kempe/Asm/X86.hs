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
import           Control.Recursion   (cata)
import           Data.Foldable.Ext
import           Data.Functor        (($>))
import           Data.Int            (Int64)
import           Data.Monoid         (Sum (..))
import           Data.Word           (Word8)
import           Kempe.AST
import           Kempe.Asm.X86.Type
import qualified Kempe.IR            as IR
import           Prettyprinter

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

expCost :: IR.Exp a -> IR.Exp Int64
expCost = cata a where
    a (IR.ConstIntF _ i)           = IR.ConstInt 0 i
    a (IR.ConstInt8F _ i)          = IR.ConstInt8 0 i
    a (IR.ConstWordF _ w)          = IR.ConstWord 0 w
    a (IR.ConstBoolF _ b)          = IR.ConstBool 0 b
    a (IR.RegF _ r)                = IR.Reg cr r
    a (IR.MemF _ sz m)             = IR.Mem (cm + IR.expCost m) sz m
    a (IR.ExprIntBinOpF _ op e e') = IR.ExprIntBinOp (2 + IR.expCost e + IR.expCost e') op e e'
    a (IR.ExprIntRelF _ op e e')   = IR.ExprIntRel (3 + IR.expCost e + IR.expCost e') op e e'

-- | Cost of a memory fetch
cm :: Int64
cm = undefined

-- | Cost of register read
cr :: Int64
cr = undefined

costToPush :: [KempeTy a] -> Int64
costToPush = undefined

-- TODO: match multiple statements
-- this isn't even recursive lmao
-- TODO: something to eval general expressions
irCosts :: IR.Stmt () -> IR.Stmt Int64
irCosts (IR.Jump _ l) = IR.Jump 1 l
irCosts (IR.KCall _ l) = IR.KCall 2 l
irCosts IR.Ret{} = IR.Ret 1
irCosts (IR.Labeled _ l) = IR.Labeled 0 l
irCosts (IR.CJump _ e@(IR.Mem _ _ (IR.ExprIntBinOp _ IR.IntPlusIR IR.Reg{} IR.ConstInt{})) l l') = IR.CJump 3 (e $> undefined) l l'
irCosts (IR.MovTemp _ r m@(IR.Mem _ _ IR.Reg{})) = IR.MovTemp 1 r (m $> undefined)
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp _ IR.IntMinusIR IR.Reg{} IR.ConstInt{})) = IR.MovTemp 1 r (e $> undefined)
irCosts (IR.MovTemp _ r e@(IR.ExprIntBinOp _ IR.IntPlusIR IR.Reg{} IR.ConstInt{})) = IR.MovTemp 1 r (e $> undefined)
irCosts (IR.MovMem _ r@IR.Reg{} sz e@(IR.ExprIntBinOp _ IR.IntMinusIR IR.Reg{} IR.Reg{})) = IR.MovMem 2 (r $> undefined) sz (e $> undefined) -- TODO: size?
irCosts (IR.MovMem _ r@IR.Reg{} sz e@IR.ConstInt{}) = IR.MovMem 1 (r $> undefined) sz (e $> undefined)
irCosts (IR.MovMem _ r@IR.Reg{} sz e@IR.ConstInt8{}) = IR.MovMem 1 (r $> undefined) sz (e $> undefined)
irCosts (IR.MovMem _ r@IR.Reg{} sz e@IR.ConstWord{}) = IR.MovMem 1 (r $> undefined) sz (e $> undefined)
irCosts (IR.MovMem _ r@IR.Reg{} sz e@(IR.ExprIntBinOp _ IR.IntTimesIR _ _)) = IR.MovMem 3 (r $> undefined) sz (e $> undefined)
irCosts (IR.MovMem _ e1@(IR.ExprIntBinOp _ _ IR.Reg{} IR.ConstInt{}) sz e2@(IR.Mem _ _ (IR.ExprIntBinOp _ IR.IntPlusIR IR.Reg{} IR.ConstInt{}))) = IR.MovMem 2 (e1 $> undefined) sz (e2 $> undefined)
irCosts (IR.MovMem _ r@IR.Reg{} sz e@(IR.ExprIntRel _ _ IR.Reg{} IR.Reg{})) = IR.MovMem 2 (r $> undefined) sz (e $> undefined)
irCosts (IR.WrapKCall _ Cabi (is, o) n l) | all (\i -> IR.size i `rem` 8 == 0) is = IR.WrapKCall (3 + sizeStack is `quot` 8) Cabi (is, o) n l
irCosts (IR.WrapKCall _ Kabi (is, os) n l) = IR.WrapKCall 3 Kabi (is, os) n l
irCosts (IR.MovTemp _ r e) = let e' = expCost e in IR.MovTemp (1 + IR.expCost e') r e'
irCosts (IR.MovMem _ e sz e') = let (e'', e''') = (expCost e, expCost e') in IR.MovMem (2 + IR.expCost e'' + IR.expCost e''') e'' sz e''' -- TODO: size?
irCosts (IR.CJump _ e l l') = let e' = expCost e in IR.CJump (2 + IR.expCost e') e' l l'
irCosts (IR.CCall _ (is, []) b) = IR.CCall (2 + costToPush is) (is, []) b
irCosts (IR.CCall _ (is, [o]) b) = IR.CCall (2 + costToPush is) (is, [o]) b
irCosts IR.CCall{} = error "C functions can have at most one return value!"

-- does this need a monad for labels/intermediaries?
irEmit :: IR.Stmt Int64 -> WriteM [X86 AbsReg ()]
irEmit (IR.Jump _ l) = pure [Jump () l]
irEmit (IR.Labeled _ l) = pure [Label () l]
irEmit (IR.KCall _ l) = pure [Call () l]
irEmit IR.Ret{} = pure [Ret ()]
irEmit (IR.CJump _ (IR.Mem _ 1 (IR.ExprIntBinOp _ IR.IntPlusIR (IR.Reg _ r) (IR.ConstInt _ i))) l l') = do
    { r' <- allocReg8
    ; pure [MovRCBool () r' 0, CmpAddrReg () (AddrRCPlus (toAbsReg r) i) r', Je () l, Jump () l']
    }
irEmit (IR.MovTemp _ r (IR.Mem _ _ (IR.Reg _ r1))) = pure [MovRA () (toAbsReg r) (Reg $ toAbsReg r1)] -- TODO: sanity check reg/mem access size?
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp _ IR.IntMinusIR (IR.Reg _ r1) (IR.ConstInt _ i))) | r == r1 =
    pure [AddRC () (toAbsReg r) i]
irEmit (IR.MovTemp _ r (IR.ExprIntBinOp _ IR.IntPlusIR (IR.Reg _ r1) (IR.ConstInt _ i))) | r == r1 =
    pure [SubRC () (toAbsReg r) i]
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ExprIntBinOp _ IR.IntMinusIR (IR.Reg _ r1) (IR.Reg _ r2))) = do -- this is a pain in the ass, maybe there is a better way to do this? -> pattern match on two sequenced instructions
    { r' <- allocReg64
    ; pure [ MovRA () r' (Reg $ toAbsReg r1), SubRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ConstInt _ i)) =
    pure [ MovAC () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ExprIntBinOp _ IR.IntTimesIR (IR.Reg _ r1) (IR.Reg _ r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), MulRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.ExprIntBinOp _ _ (IR.Reg _ r0) (IR.ConstInt _ i)) _ (IR.Mem _ 1 (IR.ExprIntBinOp _ IR.IntPlusIR (IR.Reg _ r1) (IR.ConstInt _ j)))) = do
    { r' <- allocReg8
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem _ (IR.ExprIntBinOp _ _ (IR.Reg _ r0) (IR.ConstInt _ i)) _ (IR.Mem _ 8 (IR.ExprIntBinOp _ IR.IntPlusIR (IR.Reg _ r1) (IR.ConstInt _ j)))) = do
    { r' <- allocReg64
    ; pure [ MovRA () r' (AddrRCPlus (toAbsReg r1) j), MovAR () (AddrRCPlus (toAbsReg r0) i) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ExprIntRel _ IR.IntEqIR (IR.Reg _ r1) (IR.Reg _ r2))) = do
    { l0 <- getLabel
    ; l1 <- getLabel
    ; pure [ CmpRegReg () (toAbsReg r1) (toAbsReg r2), Je () l0, Jump () l1, Label () l0, MovABool () (Reg $ toAbsReg r) 1, Label () l1, MovABool () (Reg $ toAbsReg r) 0 ]
    }
irEmit (IR.WrapKCall _ Cabi (is, [o]) n l) | all (\i -> IR.size i `rem` 8 == 0) is && IR.size o == 8 = do
    { let offs = zipWith const [1..] is
    ; let totalSize = sizeStack is + IR.size o -- for the output
    ; pure $ [BSLabel () n] ++ foldMap (\i -> [PopMem () (AddrRCPlus DataPointer (i * 8))]) offs ++ [AddRC () DataPointer totalSize, Call () l, MovAR () (AddrRCMinus DataPointer (IR.size o)) CRet, Ret ()] -- TODO: are the parameters backwards?
    -- copy last n bytes onto the system stack
    }
irEmit (IR.WrapKCall _ Kabi (_, _) n l) =
    pure [BSLabel () n, Call () l, Ret ()]
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ConstInt8 _ i)) =
    pure [ MovACi8 () (Reg $ toAbsReg r) i ]
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ConstWord _ w)) =
    pure [ MovAWord () (Reg $ toAbsReg r) w ]
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ExprIntBinOp _ IR.IntXorIR (IR.Reg _ r1) (IR.Reg _ r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), XorRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
irEmit (IR.MovMem _ (IR.Reg _ r) _ (IR.ExprIntBinOp _ IR.IntPlusIR (IR.Reg _ r1) (IR.Reg _ r2))) = do
    { r' <- allocReg64
    ; pure [ MovRR () r' (toAbsReg r1), MulRR () r' (toAbsReg r2), MovAR () (Reg $ toAbsReg r) r' ]
    }
-- total failure; try recursive back-up function at this point
irEmit (IR.MovTemp _ r e) = let e' = expCost e in evalE e' r
irEmit e = error (show $ pretty e)

-- | Code to evaluate and put some expression in a chosen 'Temp'
evalE :: IR.Exp Int64 -> IR.Temp -> WriteM [X86 AbsReg ()]
evalE (IR.ConstInt _ i) (IR.Temp64 t)  = pure [MovRC () (AllocReg64 t) i]
evalE (IR.ConstBool _ b) (IR.Temp8 t)  = pure [MovRCBool () (AllocReg8 t) (toByte b)]
evalE (IR.ConstInt8 _ i) (IR.Temp8 t)  = pure [MovRCi8 () (AllocReg8 t) i]
evalE (IR.ConstWord _ w) (IR.Temp64 t) = pure [MovRWord () (AllocReg64 t) w]

toByte :: Bool -> Word8
toByte False = 0
toByte True  = 1

sizeStack :: [KempeTy a] -> Int64
sizeStack = getSum . foldMap (Sum . IR.size)

-- I wonder if I could use a hylo.?
--
-- do both with a zipper...? or both ways idk
