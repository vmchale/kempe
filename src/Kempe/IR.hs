{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IR loosely based on Appel book.
module Kempe.IR ( writeModule
                , Stmt (..)
                , Exp (..)
                , RelBinOp (..)
                , IntBinOp (..)
                , BoolBinOp (..)
                , Label
                , Temp (..)
                , runTempM
                , TempM
                , prettyIR
                , WriteSt (..)
                , size
                ) where

import           Control.DeepSeq            (NFData)
import           Data.Foldable              (toList)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
-- strict b/c it's faster according to benchmarks
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Foldable.Ext
import           Data.Int                   (Int64, Int8)
import qualified Data.IntMap                as IM
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Word                  (Word8)
import           GHC.Generics               (Generic)
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)
import           Prettyprinter              (Doc, Pretty (pretty), braces, brackets, colon, concatWith, hardline, parens, (<+>))
import           Prettyprinter.Ext

type Label = Word

data Temp = Temp64 !Int
          | Temp8 !Int
          | DataPointer -- RBP on x86 and x19 on aarch64?
          deriving (Eq, Generic, NFData)

instance Pretty Temp where
    pretty (Temp64 i)  = "t_" <> pretty i
    pretty (Temp8 i)   = "t8_" <> pretty i
    pretty DataPointer = "datapointer"

data WriteSt = WriteSt { wlabels :: [Label]
                       , temps   :: [Int]
                       }

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

broadcastName :: Unique -> TempM Label
broadcastName (Unique i) = do
    l <- newLabel
    modifying atLabelsLens (IM.insert i l)
    pure l

lookupName :: Name a -> TempM Label
lookupName (Name _ (Unique i) _) =
    gets
        (IM.findWithDefault (error "Internal error in IR phase: could not look find label for name") i . atLabels)

prettyIR :: [Stmt] -> Doc ann
prettyIR = concatWith (\x y -> x <> hardline <> y) . fmap pretty

prettyLabel :: Label -> Doc ann
prettyLabel l = "kmp" <> pretty l

instance Pretty Stmt where
    pretty (Labeled l)           = hardline <> prettyLabel l <> colon
    pretty (Jump l)              = parens ("j" <+> prettyLabel l)
    pretty (CCall ty bs)         = parens ("C" <+> pretty (decodeUtf8 (BSL.toStrict bs)) <+> braces (prettyMonoStackType  ty))
    pretty (KCall l)             = parens ("call" <+> prettyLabel l)
    pretty Ret                   = parens "ret"
    pretty (MovTemp t e)         = parens ("movtemp" <+> pretty t <+> pretty e)
    pretty (MovMem e _ e')       = parens ("movmem" <+> pretty e <+> pretty e') -- TODO: maybe print size?
    pretty (CJump e l l')        = parens ("cjump" <+> pretty e <+> prettyLabel l <+> prettyLabel l')
    pretty (WrapKCall _ ty fn l) = hardline <> "export" <+> pretty (decodeUtf8 fn) <+> braces (prettyMonoStackType ty) <+> prettyLabel l
    pretty (MJump e l)           = parens ("mjump" <+> pretty e <+> prettyLabel l)

instance Pretty Exp where
    pretty (ConstInt i)           = parens ("int" <+> pretty i)
    pretty (ConstInt8 i)          = parens ("int8" <+> pretty i)
    pretty (ConstWord n)          = parens ("word" <+> pretty n)
    pretty (ConstBool False)      = parens "bool false"
    pretty (ConstBool True)       = parens "bool true"
    pretty (Reg t)                = parens ("reg" <+> pretty t)
    pretty (Mem sz e)             = parens ("mem" <+> brackets (pretty sz) <+> pretty e)
    pretty (ExprIntBinOp op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (ExprIntRel op e e')   = parens (pretty op <+> pretty e <+> pretty e')
    pretty (ConstTag b)           = parens ("tag" <+> prettyHex b)
    pretty (BoolBinOp op e e')    = parens (pretty op <+> pretty e <+> pretty e')
    pretty (IntNegIR e)           = parens ("~" <+> pretty e)
    pretty (PopcountIR e)         = parens ("popcount" <+> pretty e)
    pretty (EqByte e e')          = parens ("=b" <+> pretty e <+> pretty e')

data Stmt = Labeled Label
          | Jump Label
          -- conditional jump for ifs
          | CJump Exp Label Label
          | MJump Exp Label
          | CCall MonoStackType BSL.ByteString
          | KCall Label -- KCall is a jump to a Kempe procedure
          | WrapKCall ABI MonoStackType BS.ByteString Label
          | MovTemp Temp Exp -- put e in temp
          | MovMem Exp Int64 Exp -- store e2 at address given by e1
          | Ret
          deriving (Generic, NFData)

data Exp = ConstInt Int64
         | ConstInt8 Int8
         | ConstTag Word8
         | ConstWord Word
         | ConstBool Bool
         | Reg Temp -- TODO: size?
         | Mem Int64 Exp -- fetch from address
         | ExprIntBinOp IntBinOp Exp Exp
         | ExprIntRel RelBinOp Exp Exp
         | BoolBinOp BoolBinOp Exp Exp
         | IntNegIR Exp
         | PopcountIR Exp
         | EqByte Exp Exp
         deriving (Generic, NFData)
           -- TODO: one for data, one for C ABI

data BoolBinOp = BoolAnd
               | BoolOr
               | BoolXor
               deriving (Generic, NFData)

instance Pretty BoolBinOp where
    pretty BoolAnd = "&"
    pretty BoolOr  = "||"
    pretty BoolXor = "xor"

data RelBinOp = IntEqIR
              | IntNeqIR
              | IntLtIR
              | IntGtIR
              | IntLeqIR
              | IntGeqIR
              deriving (Generic, NFData)

instance Pretty RelBinOp where
    pretty IntEqIR  = "="
    pretty IntNeqIR = "!="
    pretty IntLtIR  = "<"
    pretty IntGtIR  = ">"
    pretty IntLeqIR = "<="
    pretty IntGeqIR = ">="

data IntBinOp = IntPlusIR
              | IntTimesIR
              | IntDivIR
              | IntMinusIR
              | IntModIR -- rem?
              | IntXorIR
              | WordShiftRIR -- compiles to shr on x86
              | WordShiftLIR
              -- int/word mod are different, see: https://stackoverflow.com/questions/8231882/how-to-implement-the-mod-operator-in-assembly
              | WordModIR
              | WordDivIR
              deriving (Generic, NFData)

instance Pretty IntBinOp where
    pretty IntPlusIR    = "+"
    pretty IntTimesIR   = "*"
    pretty IntDivIR     = "/"
    pretty IntMinusIR   = "-"
    pretty IntModIR     = "%"
    pretty IntXorIR     = "xor"
    pretty WordShiftRIR = ">>"
    pretty WordShiftLIR = "<<"
    pretty WordModIR    = "%~"
    pretty WordDivIR    = "/~"

writeModule :: Module () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeModule = foldMapA writeDecl

-- optimize tail-recursion, if possible
-- This is a little slow
tryTCO :: Maybe Label -> [Stmt] -> [Stmt]
tryTCO _ []           = []
tryTCO Nothing stmts  = stmts
tryTCO (Just l) stmts =
    let end = last stmts
        in
            case end of
                KCall l' | l == l' -> init stmts ++ [Jump l]
                _                  -> stmts

-- FIXME: Current broadcast + write approach fails mutually recursive functions
writeDecl :: KempeDecl () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeDecl (FunDecl _ (Name _ u _) _ _ as) = do
    bl <- broadcastName u
    (++ [Ret]) . (Labeled bl:) . tryTCO (Just bl) <$> writeAtoms (Just bl) as
writeDecl (ExtFnDecl ty (Name _ u _) _ _ cName) = do
    bl <- broadcastName u
    pure [Labeled bl, CCall ty cName, Ret]
writeDecl (Export sTy abi n) = pure . WrapKCall abi sTy (encodeUtf8 $ name n) <$> lookupName n
writeDecl TyDecl{} = error "Internal error: type declarations should not exist at this stage"

writeAtoms :: Maybe Label -> [Atom (ConsAnn MonoStackType) MonoStackType] -> TempM [Stmt]
writeAtoms _ [] = pure []
writeAtoms Nothing stmts = foldMapA (writeAtom Nothing) stmts
writeAtoms l stmts =
    let end = last stmts
        in (++) <$> foldMapA (writeAtom Nothing) (init stmts) <*> writeAtom l end

intShift :: IntBinOp -> TempM [Stmt]
intShift cons = do
    t0 <- getTemp8
    t1 <- getTemp64
    pure $
        pop 1 t0 ++ pop 8 t1 ++ push 8 (ExprIntBinOp cons (Reg t1) (Reg t0))

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
writeAtom :: Maybe Label -- ^ Context for possible TCO
          -> Atom (ConsAnn MonoStackType) MonoStackType
          -> TempM [Stmt]
writeAtom _ (IntLit _ i)              = pure $ push 8 (ConstInt $ fromInteger i)
writeAtom _ (Int8Lit _ i)             = pure $ push 1 (ConstInt8 i)
writeAtom _ (WordLit _ w)             = pure $ push 8 (ConstWord $ fromIntegral w)
writeAtom _ (BoolLit _ b)             = pure $ push 1 (ConstBool b)
writeAtom _ (AtName _ n)              = pure . KCall <$> lookupName n
writeAtom _ (AtBuiltin ([], _) Drop)  = error "Internal error: Ill-typed drop!"
writeAtom _ (AtBuiltin ([], _) Dup)   = error "Internal error: Ill-typed dup!"
writeAtom _ (Dip ([], _) _)           = error "Internal error: Ill-typed dip()!"
writeAtom _ (AtBuiltin _ IntPlus)     = intOp IntPlusIR
writeAtom _ (AtBuiltin _ IntMinus)    = intOp IntMinusIR
writeAtom _ (AtBuiltin _ IntTimes)    = intOp IntTimesIR
writeAtom _ (AtBuiltin _ IntDiv)      = intOp IntDivIR -- what to do on failure?
writeAtom _ (AtBuiltin _ IntMod)      = intOp IntModIR
writeAtom _ (AtBuiltin _ IntXor)      = intOp IntXorIR
writeAtom _ (AtBuiltin _ IntShiftR)   = intShift WordShiftRIR -- TODO: shr or sar?
writeAtom _ (AtBuiltin _ IntShiftL)   = intShift WordShiftLIR
writeAtom _ (AtBuiltin _ IntEq)       = intRel IntEqIR
writeAtom _ (AtBuiltin _ IntLt)       = intRel IntLtIR
writeAtom _ (AtBuiltin _ IntLeq)      = intRel IntLeqIR
writeAtom _ (AtBuiltin _ WordPlus)    = intOp IntPlusIR
writeAtom _ (AtBuiltin _ WordTimes)   = intOp IntTimesIR
writeAtom _ (AtBuiltin _ WordXor)     = intOp IntXorIR
writeAtom _ (AtBuiltin _ WordMinus)   = intOp IntMinusIR
writeAtom _ (AtBuiltin _ IntNeq)      = intRel IntNeqIR
writeAtom _ (AtBuiltin _ IntGeq)      = intRel IntGeqIR
writeAtom _ (AtBuiltin _ IntGt)       = intRel IntGtIR
writeAtom _ (AtBuiltin _ WordShiftL)  = intShift WordShiftLIR
writeAtom _ (AtBuiltin _ WordShiftR)  = intShift WordShiftRIR
writeAtom _ (AtBuiltin _ WordDiv)     = intOp WordDivIR
writeAtom _ (AtBuiltin _ WordMod)     = intOp WordModIR
writeAtom _ (AtBuiltin _ And)         = boolOp BoolAnd
writeAtom _ (AtBuiltin _ Or)          = boolOp BoolOr
writeAtom _ (AtBuiltin _ Xor)         = boolOp BoolXor
writeAtom _ (AtBuiltin _ IntNeg)      = intNeg
writeAtom _ (AtBuiltin _ Popcount)    = wordCount
writeAtom _ (AtBuiltin (is, _) Drop)  =
    let sz = size (last is) in
        pure [ dataPointerDec sz ]
writeAtom _ (AtBuiltin (is, _) Dup)   =
    let sz = size (last is) in
        pure $
             copyBytes 0 (-sz) sz
                ++ [ dataPointerInc sz ] -- move data pointer over sz bytes
writeAtom l (If _ as as') = do
    l0 <- newLabel
    l1 <- newLabel
    let ifIR = CJump (Mem 1 (Reg DataPointer)) l0 l1
    asIR <- tryTCO l <$> writeAtoms l as
    asIR' <- tryTCO l <$> writeAtoms l as'
    l2 <- newLabel
    pure $ dataPointerDec 1 : ifIR : (Labeled l0 : asIR ++ [Jump l2]) ++ (Labeled l1 : asIR') ++ [Labeled l2]
writeAtom _ (Dip (is, _) as) =
    let sz = size (last is)
    in foldMapA (dipify sz) as
writeAtom _ (AtBuiltin ([i0, i1], _) Swap) =
    let sz0 = size i0
        sz1 = size i1
    in
        pure $
            copyBytes 0 (-sz0 - sz1) sz0 -- copy i0 to end of the stack
                ++ copyBytes (-sz0 - sz1) (-sz1) sz1 -- copy i1 to where i0 used to be
                ++ copyBytes (-sz0) 0 sz0 -- copy i0 at end of stack to its new place
writeAtom _ (AtBuiltin _ Swap) = error "Ill-typed swap!"
writeAtom _ (AtCons ann@(ConsAnn _ tag' _) _) =
    pure $ dataPointerInc (padBytes ann) : push 1 (ConstTag tag')
writeAtom _ (Case ([], _) _) = error "Internal error: Ill-typed case statement?!"
writeAtom l (Case (is, _) ls) =
    let (ps, ass) = NE.unzip ls
        decSz = size (last is)
        in do
            leaves <- zipWithM (mkLeaf l) ps ass
            let (switches, meat) = NE.unzip leaves
            ret <- newLabel
            let meat' = (++ [Jump ret]) . toList <$> meat
            pure $ dataPointerDec decSz : concatMap toList switches ++ concat meat' ++ [Labeled ret]

zipWithM :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithM f xs ys = sequenceA (NE.zipWith f xs ys)

mkLeaf :: Maybe Label -> Pattern (ConsAnn MonoStackType) MonoStackType -> [Atom (ConsAnn MonoStackType) MonoStackType] -> TempM ([Stmt], [Stmt])
mkLeaf l p as = do
    l' <- newLabel
    as' <- writeAtoms l as
    let s = patternSwitch p l'
    pure (s, Labeled l' : as')

patternSwitch :: Pattern (ConsAnn MonoStackType) MonoStackType -> Label -> [Stmt]
patternSwitch (PatternBool _ True) l                   = [MJump (Mem 1 (Reg DataPointer)) l]
patternSwitch (PatternBool _ False) l                  = [MJump (EqByte (Mem 1 (Reg DataPointer)) (ConstTag 0)) l]
patternSwitch (PatternWildcard _) l                    = [Jump l]
patternSwitch (PatternInt _ i) l                       = [MJump (ExprIntRel IntEqIR (Mem 8 (Reg DataPointer)) (ConstInt $ fromInteger i)) l]
patternSwitch (PatternCons ann@(ConsAnn _ tag' _) _) l =
    let padAt = padBytes ann + 1
        -- decrement by padAt bytes (to discard padding), then we need to access
        -- the tag at [datapointer+padAt] when we check
        in [ dataPointerDec padAt, MJump (EqByte (Mem 1 (ExprIntBinOp IntPlusIR (Reg DataPointer) (ConstInt padAt))) (ConstTag tag')) l]

-- | Constructors may need to be padded, this computes the number of bytes of
-- padding
padBytes :: ConsAnn MonoStackType -> Int64
padBytes (ConsAnn sz _ (is, _)) = sz - sizeStack is - 1

dipify :: Int64 -> Atom (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
dipify _ (AtBuiltin ([], _) Drop) = error "Internal error: Ill-typed drop!"
dipify sz (AtBuiltin (is, _) Drop) =
    let sz' = size (last is)
        shift = dataPointerDec sz' -- shift data pointer over by sz' bytes
        -- copy sz bytes over (-sz') bytes from the data pointer
        copyBytes' = copyBytes (-sz - sz') (-sz) sz
        in pure $ copyBytes' ++ [shift]
dipify sz (AtBuiltin ([i0, i1], _) Swap) =
    let sz0 = size i0
        sz1 = size i1
    in
        pure $
            copyBytes 0 (-sz - sz0 - sz1) sz0 -- copy i0 to end of the stack
                ++ copyBytes (-sz - sz0 - sz1) (-sz - sz1) sz1 -- copy i1 to where i0 used to be
                ++ copyBytes (-sz - sz0) 0 sz0 -- copy i0 at end of stack to its new place
dipify _ (Dip ([], _) _) = error "Internal error: Ill-typed dip()!"
dipify sz (Dip (is, _) as) =
    let sz' = size (last is)
        in foldMapA (dipify (sz + sz')) as
dipify _ (AtBuiltin _ Swap)        = error "Internal error: Ill-typed swap!"
dipify sz (AtBuiltin _ IntTimes)   = dipOp sz IntTimesIR
dipify sz (AtBuiltin _ IntPlus)    = dipOp sz IntPlusIR
dipify sz (AtBuiltin _ IntMinus)   = dipOp sz IntMinusIR
dipify sz (AtBuiltin _ IntDiv)     = dipOp sz IntDivIR
dipify sz (AtBuiltin _ IntMod)     = dipOp sz IntModIR
dipify sz (AtBuiltin _ IntXor)     = dipOp sz IntXorIR
dipify sz (AtBuiltin _ IntEq)      = dipRel sz IntEqIR
dipify sz (AtBuiltin _ IntLt)      = dipRel sz IntLtIR
dipify sz (AtBuiltin _ IntLeq)     = dipRel sz IntLeqIR
dipify sz (AtBuiltin _ IntShiftL)  = dipShift sz WordShiftLIR
dipify sz (AtBuiltin _ IntShiftR)  = dipShift sz WordShiftRIR
dipify sz (AtBuiltin _ WordXor)    = dipOp sz IntXorIR
dipify sz (AtBuiltin _ WordShiftL) = dipShift sz WordShiftLIR
dipify sz (AtBuiltin _ WordShiftR) = dipShift sz WordShiftRIR
dipify sz (AtBuiltin _ WordPlus)   = dipOp sz IntPlusIR
dipify sz (AtBuiltin _ WordTimes)  = dipOp sz IntTimesIR
dipify sz (AtBuiltin _ IntGeq)     = dipRel sz IntGeqIR
dipify sz (AtBuiltin _ IntGt)      = dipRel sz IntGtIR
dipify sz (AtBuiltin _ IntNeq)     = dipRel sz IntNeqIR
dipify sz (AtBuiltin _ IntNeg)     = plainShift sz <$> intNeg
dipify sz (AtBuiltin _ Popcount)   = plainShift sz <$> wordCount
dipify sz (AtBuiltin _ And)        = dipBoolOp sz BoolAnd
dipify sz (AtBuiltin _ Or)         = dipBoolOp sz BoolOr
dipify sz (AtBuiltin _ Xor)        = dipBoolOp sz BoolXor
dipify sz (AtBuiltin _ WordMinus)  = dipOp sz IntMinusIR
dipify sz (AtBuiltin _ WordDiv)    = dipOp sz WordDivIR
dipify sz (AtBuiltin _ WordMod)    = dipOp sz WordModIR
dipify _ (AtBuiltin ([], _) Dup) = error "Internal error: Ill-typed dup!"
dipify sz (AtBuiltin (is, _) Dup) = do
    let sz' = size (last is) in
        pure $
             copyBytes 0 (-sz) sz -- copy sz bytes over to the end of the stack
                ++ copyBytes (-sz) (-sz - sz') sz' -- copy sz' bytes over (duplicate)
                ++ copyBytes (-sz + sz') 0 sz -- copy sz bytes back
                ++ [ dataPointerInc sz' ] -- move data pointer over sz' bytes
dipify sz (IntLit _ i) = pure $ dipPush sz 8 (ConstInt $ fromInteger i)
dipify sz (WordLit _ w) = pure $ dipPush sz 8 (ConstWord $ fromIntegral w)
dipify sz (Int8Lit _ i) = pure $ dipPush sz 1 (ConstInt8 i)
dipify sz (BoolLit _ b) = pure $ dipPush sz 1 (ConstBool b)
dipify sz (AtCons ann@(ConsAnn _ tag' _) _) =
    pure $
        copyBytes 0 (-sz) sz
            ++ dataPointerInc (padBytes ann) : push 1 (ConstTag tag')
            ++ copyBytes (-sz) 0 sz
dipify sz a@(If sty _ _) =
    dipSupp sz sty <$> writeAtom Nothing a
dipify sz (AtName sty n) =
    dipSupp sz sty . pure . KCall <$> lookupName n
dipify sz a@(Case sty _) =
    dipSupp sz sty <$> writeAtom Nothing a

dipSupp :: Int64 -> MonoStackType -> [Stmt] -> [Stmt]
dipSupp sz (is, os) stmts =
    let excessSz = sizeStack os - sizeStack is -- how much the atom(s) grow the stack
        in case compare excessSz 0 of
            EQ -> plainShift sz stmts
            LT -> dipDo sz stmts

dipPush :: Int64 -> Int64 -> Exp -> [Stmt]
dipPush sz sz' e =
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
copyBytes off1 off2 b
    | b `mod` 8 == 0 =
        let is = fmap (8*) [0..(b `div` 8 - 1)] in
            [ MovMem (dataPointerPlus (i + off1)) 8 (Mem 8 $ dataPointerPlus (i + off2)) | i <- is ]
    | otherwise =
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
