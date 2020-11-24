module Kempe.IR.Opt ( optimize
                    ) where

import           Kempe.IR

optimize :: [Stmt] -> [Stmt]
optimize = successiveBumps

-- | Often IR generation will leave us with something like
--
-- > (movtemp datapointer (+ (reg datapointer) (int 8)))
-- > (movtemp datapointer (- (reg datapointer) (int 8)))
--
-- i.e. push a value and immediately pop it for use.
--
-- This is silly and we remove it in this pass.
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
successiveBumps (s:ss) = s : successiveBumps ss
