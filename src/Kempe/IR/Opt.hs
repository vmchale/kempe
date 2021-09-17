module Kempe.IR.Opt ( optimize
                    ) where

import           Kempe.IR.Type

optimize :: [Stmt] -> [Stmt]
optimize = sameTarget . successiveBumps . successiveBumps . removeNop . liftOptE

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
    ((MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i')))
        :ss) =
            MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt $ i-i')) : successiveBumps ss
successiveBumps
    ((MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i)))
        :(MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i')))
        :ss) =
            MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt $ i-i')) : successiveBumps ss
successiveBumps
    (st@(MovMem e0 k (Mem 8 e1))
        :(MovMem e0' k' (Mem 8 e1'))
        :ss) | k == k' && e0 == e1' && e1 == e0' = st : successiveBumps ss
successiveBumps (s:ss) = s : successiveBumps ss

-- | Stuff like
--
-- > (movmem (- (reg datapointer) (int 8)) (mem [8] (- (reg datapointer) (int 0))))
-- > (movmem (- (reg datapointer) (int 8)) (mem [8] (- (reg datapointer) (int 16))))
--
-- Basically if two successive 'Stmt's write to the same location, only bother
-- with the second one.
sameTarget :: [Stmt] -> [Stmt]
sameTarget [] = []
sameTarget
    ((MovMem e0 k _)
        :st@(MovMem e0' k' _)
        :ss) | k == k' && e0 == e0' = st : sameTarget ss
sameTarget (s:ss) = s : sameTarget ss

liftOptE :: [Stmt] -> [Stmt]
liftOptE []                       = []
liftOptE ((MovMem e0 sz e1) : ss) = MovMem (optE e0) sz (optE e1) : liftOptE ss
liftOptE ((MovTemp t e) : ss)     = MovTemp t (optE e) : liftOptE ss
liftOptE (s:ss)                   = s : liftOptE ss

optE :: Exp -> Exp
optE (ExprIntBinOp IntPlusIR e (ConstInt 0))  = optE e
optE (ExprIntBinOp IntMinusIR e (ConstInt 0)) = optE e
optE e                                        = e

removeNop :: [Stmt] -> [Stmt]
removeNop = filter (not . isNop)
    where
        isNop (MovTemp e (Reg e')) | e == e' = True
        isNop (MovMem e _ (Mem _ e')) | e == e' = True -- the Eq on Exp is kinda weird, but if the syntax trees are the same then they're certainly equivalent semantically
        isNop _ = False
