module Kempe.IR ( writeModule
                , runTempM
                , TempM
                , prettyIR
                , WriteSt (..)
                , size
                ) where

import           Data.Foldable              (toList, traverse_)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
-- strict b/c it's faster according to benchmarks
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor             (second)
import           Data.Foldable.Ext
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           Data.Text.Encoding         (encodeUtf8)
import           Kempe.AST
import           Kempe.AST.Size
import           Kempe.IR.Type
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)
import           Prettyprinter              (Doc, Pretty (pretty))
import           Prettyprinter.Ext

data TempSt = TempSt { labels     :: [Label]
                     , tempSupply :: [Int]
                     , atLabels   :: IM.IntMap Label
                     -- TODO: type sizes in state
                     }

asWriteSt :: TempSt -> WriteSt
asWriteSt (TempSt ls ts _) = WriteSt ls ts

runTempM :: TempM a -> (a, WriteSt)
runTempM = second asWriteSt . flip runState (TempSt [1..] [1..] mempty)

atLabelsLens :: Lens' TempSt (IM.IntMap Label)
atLabelsLens f s = fmap (\x -> s { atLabels = x }) (f (atLabels s))

nextLabels :: TempSt -> TempSt
nextLabels (TempSt ls ts ats) = TempSt (tail ls) ts ats

nextTemps :: TempSt -> TempSt
nextTemps (TempSt ls ts ats) = TempSt ls (tail ts) ats

type TempM = State TempSt

getTemp :: TempM Int
getTemp = gets (head . tempSupply) <* modify nextTemps

getTemp64 :: TempM Temp
getTemp64 = Temp64 <$> getTemp

getTemp8 :: TempM Temp
getTemp8 = Temp8 <$> getTemp

newLabel :: TempM Label
newLabel = gets (head . labels) <* modify nextLabels

broadcastName :: Unique -> TempM ()
broadcastName (Unique i) = do
    l <- newLabel
    modifying atLabelsLens (IM.insert i l)

lookupName :: Name a -> TempM Label
lookupName (Name _ (Unique i) _) =
    gets
        (IM.findWithDefault (error "Internal error in IR phase: could not look find label for name") i . atLabels)

prettyIR :: [Stmt] -> Doc ann
prettyIR = prettyLines . fmap pretty

writeModule :: SizeEnv -> Declarations () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeModule env m = traverse_ assignName m *> foldMapA (writeDecl env) m

-- optimize tail-recursion, if possible
-- This is a little slow
tryTCO :: Bool -- ^ Can it be optimized here?
       -> [Stmt]
       -> [Stmt]
tryTCO _ []           = []
tryTCO False stmts  = stmts
tryTCO True stmts =
    let end = last stmts
        in
            case end of
                KCall l' -> init stmts ++ [Jump l']
                _        -> stmts

assignName :: KempeDecl a c b -> TempM ()
assignName (FunDecl _ (Name _ u _) _ _ _)   = broadcastName u
assignName (ExtFnDecl _ (Name _ u _) _ _ _) = broadcastName u
assignName Export{}                         = pure ()
assignName TyDecl{}                         = error "Internal error: type declarations should not exist at this stage"

writeDecl :: SizeEnv -> KempeDecl () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeDecl env (FunDecl _ n _ _ as) = do
    bl <- lookupName n
    (++ [Ret]) . (Labeled bl:) . tryTCO True <$> writeAtoms env True as
writeDecl _ (ExtFnDecl ty n _ _ cName) = do
    bl <- lookupName n
    pure [Labeled bl, CCall ty cName, Ret]
writeDecl _ (Export sTy abi n) = pure . WrapKCall abi sTy (encodeUtf8 $ name n) <$> lookupName n
writeDecl _ TyDecl{} = error "Internal error: type declarations should not exist at this stage"

writeAtoms :: SizeEnv -> Bool -> [Atom (ConsAnn MonoStackType) MonoStackType] -> TempM [Stmt]
writeAtoms _ _ [] = pure []
writeAtoms env False stmts = foldMapA (writeAtom env False) stmts
writeAtoms env l stmts =
    let end = last stmts
        in (++) <$> foldMapA (writeAtom env False) (init stmts) <*> writeAtom env l end

intShift :: IntBinOp -> TempM [Stmt]
intShift cons = do
    t0 <- getTemp64
    t1 <- getTemp64
    pure $
        pop 8 t0 ++ pop 8 t1 ++ push 8 (ExprIntBinOp cons (Reg t1) (Reg t0))

boolOp :: BoolBinOp -> TempM [Stmt]
boolOp op = do
    t0 <- getTemp8
    t1 <- getTemp8
    pure $
        pop 1 t0 ++ pop 1 t1 ++ push 1 (BoolBinOp op (Reg t1) (Reg t0))

intOp :: IntBinOp -> TempM [Stmt]
intOp cons = do
    t0 <- getTemp64 -- registers are 64 bits for integers
    t1 <- getTemp64
    pure $
        pop 8 t0 ++ pop 8 t1 ++ push 8 (ExprIntBinOp cons (Reg t1) (Reg t0))

-- | Push bytes onto the Kempe data pointer
push :: Int64 -> Exp -> [Stmt]
push off e =
    [ MovMem (Reg DataPointer) off e
    , dataPointerInc off -- increment instead of decrement b/c this is the Kempe ABI
    ]

pop :: Int64 -> Temp -> [Stmt]
pop sz t =
    [ dataPointerDec sz
    , MovTemp t (Mem sz (Reg DataPointer))
    ]

-- FIXME: just use expressions from memory accesses
intRel :: RelBinOp -> TempM [Stmt]
intRel cons = do
    t0 <- getTemp64
    t1 <- getTemp64
    pure $
        pop 8 t0 ++ pop 8 t1 ++ push 1 (ExprIntRel cons (Reg t1) (Reg t0))

intNeg :: TempM [Stmt]
intNeg = do
    t0 <- getTemp64
    pure $
        pop 8 t0 ++ push 8 (IntNegIR (Reg t0))

wordCount :: TempM [Stmt]
wordCount = do
    t0 <- getTemp64
    pure $
        pop 8 t0 ++ push 8 (PopcountIR (Reg t0))

-- | This throws exceptions on nonsensical input.
writeAtom :: SizeEnv
          -> Bool -- ^ Can we do TCO?
          -> Atom (ConsAnn MonoStackType) MonoStackType
          -> TempM [Stmt]
writeAtom _ _ (IntLit _ i)              = pure $ push 8 (ConstInt $ fromInteger i)
writeAtom _ _ (Int8Lit _ i)             = pure $ push 1 (ConstInt8 i)
writeAtom _ _ (WordLit _ w)             = pure $ push 8 (ConstWord $ fromIntegral w)
writeAtom _ _ (BoolLit _ b)             = pure $ push 1 (ConstBool b)
writeAtom _ _ (AtName _ n)              = pure . KCall <$> lookupName n
writeAtom _ _ (AtBuiltin ([], _) Drop)  = error "Internal error: Ill-typed drop!"
writeAtom _ _ (AtBuiltin ([], _) Dup)   = error "Internal error: Ill-typed dup!"
writeAtom _ _ (Dip ([], _) _)           = error "Internal error: Ill-typed dip()!"
writeAtom _ _ (AtBuiltin _ IntPlus)     = intOp IntPlusIR
writeAtom _ _ (AtBuiltin _ IntMinus)    = intOp IntMinusIR
writeAtom _ _ (AtBuiltin _ IntTimes)    = intOp IntTimesIR
writeAtom _ _ (AtBuiltin _ IntDiv)      = intOp IntDivIR -- what to do on failure?
writeAtom _ _ (AtBuiltin _ IntMod)      = intOp IntModIR
writeAtom _ _ (AtBuiltin _ IntXor)      = intOp IntXorIR
writeAtom _ _ (AtBuiltin _ IntShiftR)   = intShift WordShiftRIR -- FIXME: shr or sar?
writeAtom _ _ (AtBuiltin _ IntShiftL)   = intShift WordShiftLIR
writeAtom _ _ (AtBuiltin _ IntEq)       = intRel IntEqIR
writeAtom _ _ (AtBuiltin _ IntLt)       = intRel IntLtIR
writeAtom _ _ (AtBuiltin _ IntLeq)      = intRel IntLeqIR
writeAtom _ _ (AtBuiltin _ WordPlus)    = intOp IntPlusIR
writeAtom _ _ (AtBuiltin _ WordTimes)   = intOp IntTimesIR
writeAtom _ _ (AtBuiltin _ WordXor)     = intOp IntXorIR
writeAtom _ _ (AtBuiltin _ WordMinus)   = intOp IntMinusIR
writeAtom _ _ (AtBuiltin _ IntNeq)      = intRel IntNeqIR
writeAtom _ _ (AtBuiltin _ IntGeq)      = intRel IntGeqIR
writeAtom _ _ (AtBuiltin _ IntGt)       = intRel IntGtIR
writeAtom _ _ (AtBuiltin _ WordShiftL)  = intShift WordShiftLIR
writeAtom _ _ (AtBuiltin _ WordShiftR)  = intShift WordShiftRIR
writeAtom _ _ (AtBuiltin _ WordDiv)     = intOp WordDivIR
writeAtom _ _ (AtBuiltin _ WordMod)     = intOp WordModIR
writeAtom _ _ (AtBuiltin _ And)         = boolOp BoolAnd
writeAtom _ _ (AtBuiltin _ Or)          = boolOp BoolOr
writeAtom _ _ (AtBuiltin _ Xor)         = boolOp BoolXor
writeAtom _ _ (AtBuiltin _ IntNeg)      = intNeg
writeAtom _ _ (AtBuiltin _ Popcount)    = wordCount
writeAtom env _ (AtBuiltin (is, _) Drop)  =
    let sz = size' env (last is) in
        pure [ dataPointerDec sz ]
writeAtom env _ (AtBuiltin (is, _) Dup)   =
    let sz = size' env (last is) in
        pure $
             copyBytes 0 (-sz) sz
                ++ [ dataPointerInc sz ] -- move data pointer over sz bytes
writeAtom env l (If _ as as') = do
    l0 <- newLabel
    l1 <- newLabel
    let ifIR = CJump (Mem 1 (Reg DataPointer)) l0 l1
    asIR <- tryTCO l <$> writeAtoms env l as
    asIR' <- tryTCO l <$> writeAtoms env l as'
    l2 <- newLabel
    pure $ dataPointerDec 1 : ifIR : (Labeled l0 : asIR ++ [Jump l2]) ++ (Labeled l1 : asIR') ++ [Labeled l2]
writeAtom env _ (Dip (is, _) as) =
    let sz = size' env (last is)
    in foldMapA (dipify env sz) as
writeAtom env _ (AtBuiltin ([i0, i1], _) Swap) =
    let sz0 = size' env i0
        sz1 = size' env i1
    in
        pure $
            copyBytes 0 (-sz0 - sz1) sz0 -- copy i0 to end of the stack
                ++ copyBytes (-sz0 - sz1) (-sz1) sz1 -- copy i1 to where i0 used to be
                ++ copyBytes (-sz0) 0 sz0 -- copy i0 at end of stack to its new place
writeAtom _ _ (AtBuiltin _ Swap) = error "Ill-typed swap!"
writeAtom env _ (AtCons ann@(ConsAnn _ tag' _) _) =
    pure $ dataPointerInc (padBytes env ann) : push 1 (ConstTag tag')
writeAtom _ _ (Case ([], _) _) = error "Internal error: Ill-typed case statement?!"
-- single-case leaf
writeAtom env l (Case (is, _) ((_, as) :| [])) =
    let decSz = size' env (last is)
    in do
        nextAs <- writeAtoms env l as
        pure $ dataPointerDec decSz : nextAs
writeAtom env l (Case (is, _) ls) =
    let (ps, ass) = NE.unzip ls
        decSz = size' env (last is)
        in do
            leaves <- zipWithM (mkLeaf env l) ps ass
            let (switches, meat) = NE.unzip leaves
            ret <- newLabel
            let meat' = (++ [Jump ret]) . toList <$> meat
            pure $ dataPointerDec decSz : concatMap toList switches ++ concat meat' ++ [Labeled ret]
            -- TODO: why dataPointerDec decSz??

zipWithM :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys = sequenceA (NE.zipWith f xs ys)

mkLeaf :: SizeEnv -> Bool -> Pattern (ConsAnn MonoStackType) MonoStackType -> [Atom (ConsAnn MonoStackType) MonoStackType] -> TempM ([Stmt], [Stmt])
mkLeaf env l p as = do
    l' <- newLabel
    as' <- writeAtoms env l as
    let s = patternSwitch env p l'
    pure (s, Labeled l' : as')

patternSwitch :: SizeEnv -> Pattern (ConsAnn MonoStackType) MonoStackType -> Label -> [Stmt]
patternSwitch _ (PatternBool _ True) l                   = [MJump (Mem 1 (Reg DataPointer)) l]
patternSwitch _ (PatternBool _ False) l                  = [MJump (EqByte (Mem 1 (Reg DataPointer)) (ConstTag 0)) l]
patternSwitch _ (PatternWildcard _) l                    = [Jump l] -- FIXME: what about padding? when standing in for a constructor...
patternSwitch _ (PatternInt _ i) l                       = [MJump (ExprIntRel IntEqIR (Mem 8 (Reg DataPointer)) (ConstInt $ fromInteger i)) l]
patternSwitch env (PatternCons ann@(ConsAnn _ tag' _) _) l =
    let padAt = padBytes env ann -- + 1
        -- decrement by padAt bytes (to discard padding), then we need to access
        -- the tag at [datapointer+padAt] when we check
        in [ MJump (EqByte (Mem 1 (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt padAt))) (ConstTag tag')) l]
        -- FIXME: do we need dataPointerInc padAt at the end? not all
        -- constructors will have the same padding, it will fall through...

-- | Constructors may need to be padded, this computes the number of bytes of
-- padding
padBytes :: SizeEnv -> ConsAnn MonoStackType -> Int64
padBytes env (ConsAnn sz _ (is, _)) = sz - sizeStack env is - 1

dipify :: SizeEnv -> Int64 -> Atom (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
dipify _ _ (AtBuiltin ([], _) Drop) = error "Internal error: Ill-typed drop!"
dipify env sz (AtBuiltin (is, _) Drop) =
    let sz' = size' env (last is)
        shift = dataPointerDec sz' -- shift data pointer over by sz' bytes
        -- copy sz bytes over (-sz') bytes from the data pointer
        copyBytes' = copyBytes (-sz - sz') (-sz) sz
        in pure $ copyBytes' ++ [shift]
dipify env sz (AtBuiltin ([i0, i1], _) Swap) =
    let sz0 = size' env i0
        sz1 = size' env i1
    in
        pure $
            copyBytes 0 (-sz - sz0 - sz1) sz0 -- copy i0 to end of the stack
                ++ copyBytes (-sz - sz0 - sz1) (-sz - sz1) sz1 -- copy i1 to where i0 used to be
                ++ copyBytes (-sz - sz0) 0 sz0 -- copy i0 at end of stack to its new place
dipify _ _ (Dip ([], _) _) = error "Internal error: Ill-typed dip()!"
dipify env sz (Dip (is, _) as) =
    let sz' = size' env (last is)
        in foldMapA (dipify env (sz + sz')) as
dipify _ _ (AtBuiltin _ Swap)        = error "Internal error: Ill-typed swap!"
dipify _ sz (AtBuiltin _ IntTimes)   = dipOp sz IntTimesIR
dipify _ sz (AtBuiltin _ IntPlus)    = dipOp sz IntPlusIR
dipify _ sz (AtBuiltin _ IntMinus)   = dipOp sz IntMinusIR
dipify _ sz (AtBuiltin _ IntDiv)     = dipOp sz IntDivIR
dipify _ sz (AtBuiltin _ IntMod)     = dipOp sz IntModIR
dipify _ sz (AtBuiltin _ IntXor)     = dipOp sz IntXorIR
dipify _ sz (AtBuiltin _ IntEq)      = dipRel sz IntEqIR
dipify _ sz (AtBuiltin _ IntLt)      = dipRel sz IntLtIR
dipify _ sz (AtBuiltin _ IntLeq)     = dipRel sz IntLeqIR
dipify _ sz (AtBuiltin _ IntShiftL)  = dipShift sz WordShiftLIR
dipify _ sz (AtBuiltin _ IntShiftR)  = dipShift sz WordShiftRIR
dipify _ sz (AtBuiltin _ WordXor)    = dipOp sz IntXorIR
dipify _ sz (AtBuiltin _ WordShiftL) = dipShift sz WordShiftLIR
dipify _ sz (AtBuiltin _ WordShiftR) = dipShift sz WordShiftRIR
dipify _ sz (AtBuiltin _ WordPlus)   = dipOp sz IntPlusIR
dipify _ sz (AtBuiltin _ WordTimes)  = dipOp sz IntTimesIR
dipify _ sz (AtBuiltin _ IntGeq)     = dipRel sz IntGeqIR
dipify _ sz (AtBuiltin _ IntGt)      = dipRel sz IntGtIR
dipify _ sz (AtBuiltin _ IntNeq)     = dipRel sz IntNeqIR
dipify _ sz (AtBuiltin _ IntNeg)     = plainShift sz <$> intNeg
dipify _ sz (AtBuiltin _ Popcount)   = plainShift sz <$> wordCount
dipify _ sz (AtBuiltin _ And)        = dipBoolOp sz BoolAnd
dipify _ sz (AtBuiltin _ Or)         = dipBoolOp sz BoolOr
dipify _ sz (AtBuiltin _ Xor)        = dipBoolOp sz BoolXor
dipify _ sz (AtBuiltin _ WordMinus)  = dipOp sz IntMinusIR
dipify _ sz (AtBuiltin _ WordDiv)    = dipOp sz WordDivIR
dipify _ sz (AtBuiltin _ WordMod)    = dipOp sz WordModIR
dipify _ _ (AtBuiltin ([], _) Dup) = error "Internal error: Ill-typed dup!"
dipify env sz (AtBuiltin (is, _) Dup) = do
    let sz' = size' env (last is) in
        pure $
             copyBytes 0 (-sz) sz -- copy sz bytes over to the end of the stack
                ++ copyBytes (-sz) (-sz - sz') sz' -- copy sz' bytes over (duplicate)
                ++ copyBytes (-sz + sz') 0 sz -- copy sz bytes back
                ++ [ dataPointerInc sz' ] -- move data pointer over sz' bytes
dipify _ sz (IntLit _ i) = pure $ dipPush sz 8 (ConstInt $ fromInteger i)
dipify _ sz (WordLit _ w) = pure $ dipPush sz 8 (ConstWord $ fromIntegral w)
dipify _ sz (Int8Lit _ i) = pure $ dipPush sz 1 (ConstInt8 i)
dipify _ sz (BoolLit _ b) = pure $ dipPush sz 1 (ConstBool b)
dipify env sz (AtCons ann@(ConsAnn _ tag' _) _) =
    pure $
        copyBytes 0 (-sz) sz
            ++ dataPointerInc (padBytes env ann) : push 1 (ConstTag tag')
            ++ copyBytes (-sz) 0 sz
dipify env sz a@(If sty _ _) =
    dipSupp env sz sty <$> writeAtom env False a
dipify env sz (AtName sty n) =
    dipSupp env sz sty . pure . KCall <$> lookupName n
dipify env sz a@(Case sty _) =
    dipSupp env sz sty <$> writeAtom env False a

dipSupp :: SizeEnv -> Int64 -> MonoStackType -> [Stmt] -> [Stmt]
dipSupp env sz (is, os) stmts =
    let excessSz = sizeStack env os - sizeStack env is -- how much the atom(s) grow the stack
        in case compare excessSz 0 of
            EQ -> plainShift sz stmts
            LT -> dipDo sz stmts
            GT -> dipHelp excessSz sz stmts

dipHelp :: Int64 -> Int64 -> [Stmt] -> [Stmt]
dipHelp excessSz dipSz stmts =
    let shiftNext = dataPointerDec dipSz
        shiftBack = dataPointerInc dipSz
    in
    shiftNext
        : copyBytes excessSz (-dipSz) dipSz -- copy bytes past end of stack
        ++ stmts
        ++ copyBytes (-dipSz) 0 dipSz -- copy bytes back (now from 0 of stack; data pointer has been set)
        ++ [shiftBack]

dipPush :: Int64 -> Int64 -> Exp -> [Stmt]
dipPush sz sz' e =
    -- FIXME: is this right?
    copyBytes 0 (-sz) sz
        ++ push sz' e
        ++ copyBytes (-sz) 0 sz -- copy bytes back (data pointer has been incremented already by push)

-- for e.g. negation where the stack size stays the same
plainShift :: Int64 -> [Stmt] -> [Stmt]
plainShift sz stmt =
    let shiftNext = dataPointerDec sz
        shiftBack = dataPointerInc sz
    in
        (shiftNext : stmt ++ [shiftBack])

-- works in general because relations, shifts, operations shrink the size of the
-- stack.
dipDo :: Int64 -> [Stmt] -> [Stmt]
dipDo sz stmt =
    let shiftNext = dataPointerDec sz
        shiftBack = dataPointerInc sz
        copyBytes' = copyBytes 0 sz sz
    in
        (shiftNext : stmt ++ copyBytes' ++ [shiftBack])

dipShift :: Int64 -> IntBinOp -> TempM [Stmt]
dipShift sz op = dipDo sz <$> intShift op

dipRel :: Int64 -> RelBinOp -> TempM [Stmt]
dipRel sz rel = dipDo sz <$> intRel rel

dipOp :: Int64 -> IntBinOp -> TempM [Stmt]
dipOp sz op = dipDo sz <$> intOp op

dipBoolOp :: Int64 -> BoolBinOp -> TempM [Stmt]
dipBoolOp sz op = dipDo sz <$> boolOp op

copyBytes :: Int64 -- ^ dest offset
          -> Int64 -- ^ src offset
          -> Int64 -- ^ Number of bytes to copy
          -> [Stmt]
copyBytes off1 off2 b =
    let (b8, b1) = b `quotRem` 8
        in copyBytes8 off1 off2 b8 ++ copyBytes1 (off1 + b8 * 8) (off2 + b8 * 8) b1
        -- copyBytesPlain

-- | Copy bytes 8 at a time. Note that @b@ must be divisible by 8.
copyBytes8 :: Int64
           -> Int64
           -> Int64 -- ^ @b@ (number 8-byte chunks to copy)
           -> [Stmt]
copyBytes8 off1 off2 b =
    let is = fmap (8*) [0..(b - 1)] in
        [ MovMem (dataPointerPlus (i + off1)) 8 (Mem 8 $ dataPointerPlus (i + off2)) | i <- is ]

copyBytes1 :: Int64 -> Int64 -> Int64 -> [Stmt]
copyBytes1 off1 off2 b =
    [ MovMem (dataPointerPlus (i + off1)) 1 (Mem 1 $ dataPointerPlus (i + off2)) | i <- [0..(b-1)] ]

dataPointerDec :: Int64 -> Stmt
dataPointerDec i = MovTemp DataPointer (ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt i))

dataPointerInc :: Int64 -> Stmt
dataPointerInc i = MovTemp DataPointer (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt i))

dataPointerPlus :: Int64 -> Exp
dataPointerPlus off =
    if off > 0
        then ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt off)
        else ExprIntBinOp IntMinusIR (Reg DataPointer) (ConstInt (negate off))
