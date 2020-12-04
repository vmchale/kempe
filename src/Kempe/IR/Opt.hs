module Kempe.IR.Opt ( optimize
                    ) where

import           Kempe.IR

optimize :: [Stmt] -> [Stmt]
optimize = successiveBumps . removeNop

-- | Often IR generation will leave us with something like
--
-- > (movtemp datapointer (+ (reg datapointer) (int 8)))
-- > (movtemp datapointer (- (reg datapointer) (int 8)))
--
-- i.e. push a value and immediately pop it for use.
--
-- This is silly and we remove it in this pass.
--
-- Also take the opportunity to simplify stuff like
--
-- > (movmem (- (reg datapointer) (int 8)) (mem [8] (- (reg datapointer) (int 0))))
-- > (movmem (- (reg datapointer) (int 0)) (mem [8] (- (reg datapointer) (int 8))))
successiveBumps :: [Stmt] -> [Stmt]
successiveBumps [] = []
successiveBumps
    ((MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i')))
        :ss) | i == i' = successiveBumps ss
successiveBumps
    ((MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i')))
        :ss) | i == i' = successiveBumps ss
successiveBumps
    ((MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i')))
        :ss) =
            MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt $ i+i')) : successiveBumps ss
successiveBumps
    ((MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i')))
        :ss) =
            MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt $ i+i')) : successiveBumps ss
successiveBumps
    (st@(MovMem e0 k (Mem 8 e1))
        :(MovMem e0' k' (Mem 8 e1'))
        :ss) | k == k' && e0 == e1' && e1 == e0' = st : successiveBumps ss
successiveBumps (s:ss) = s : successiveBumps ss

removeNop :: [Stmt] -> [Stmt]
removeNop = filter (not . isNop)
    where
        isNop (MovMem e _ (Mem _ e')) | e == e' = True -- the Eq on Exp is kinda weird, but if the syntax trees are the same then they're certainly equivalent semantically
        isNop _ = False
