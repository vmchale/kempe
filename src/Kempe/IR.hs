{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IR loosely based on Appel book.
module Kempe.IR ( writeModule
                , Stmt (..)
                , Exp (..)
                , RelBinOp (..)
                , IntBinOp (..)
                , Label
                , Temp (..)
                , runTempM
                , TempM
                , prettyIR
                , WriteSt (..)
                , size
                ) where

import           Control.DeepSeq            (NFData)
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
import           Prettyprinter              (Doc, Pretty (pretty), braces, colon, concatWith, hardline, parens, (<+>))

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

instance Pretty Exp where
    pretty (ConstInt i)           = parens ("int" <+> pretty i)
    pretty (ConstInt8 i)          = parens ("int8" <+> pretty i)
    pretty (ConstWord n)          = parens ("word" <+> pretty n)
    pretty (ConstBool False)      = parens "bool false"
    pretty (ConstBool True)       = parens "bool true"
    pretty (Reg t)                = parens ("reg" <+> pretty t)
    pretty (Mem _ e)              = parens ("mem" <+> pretty e)
    pretty (ExprIntBinOp op e e') = parens (pretty op <+> pretty e <+> pretty e')
    pretty (ExprIntRel op e e')   = parens (pretty op <+> pretty e <+> pretty e')

data Stmt = Labeled Label
          | Jump Label
          -- conditional jump for ifs
          | CJump Exp Label Label
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
         deriving (Generic, NFData)
           -- TODO: one for data, one for C ABI

data RelBinOp = IntEqIR
              | IntNeqIR
              | IntLtIR
              | IntGtIR
              deriving (Generic, NFData)

instance Pretty RelBinOp where
    pretty IntEqIR  = "="
    pretty IntNeqIR = "!="
    pretty IntLtIR  = "<"
    pretty IntGtIR  = ">"

data IntBinOp = IntPlusIR
              | IntTimesIR
              | IntDivIR
              | IntMinusIR
              | IntModIR -- rem?
              | IntXorIR
              | WordTimesIR
              | WordShiftRIR -- compiles to shr on x86
              | WordShiftLIR
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
    pretty WordTimesIR  = "*~"

writeModule :: Module () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeModule = foldMapA writeDecl

-- FIXME: Current broadcast + write approach fails mutually recursive functions
writeDecl :: KempeDecl () (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeDecl (FunDecl _ (Name _ u _) _ _ as) = do
    bl <- broadcastName u
    (++ [Ret]) . (Labeled bl:) <$> writeAtoms as
writeDecl (ExtFnDecl ty (Name _ u _) _ _ cName) = do
    bl <- broadcastName u
    pure [Labeled bl, CCall ty cName, Ret]
writeDecl (Export sTy abi n) = pure . WrapKCall abi sTy (encodeUtf8 $ name n) <$> lookupName n

writeAtoms :: [Atom (ConsAnn MonoStackType) MonoStackType] -> TempM [Stmt]
writeAtoms = foldMapA writeAtom

intShift :: IntBinOp -> TempM [Stmt]
intShift cons = do
    t0 <- getTemp8
    t1 <- getTemp64
    pure $
        pop 1 t0 ++ pop 8 t1 ++ push 8 (ExprIntBinOp cons (Reg t1) (Reg t0))

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

-- | This throws exceptions on nonsensical input.
writeAtom :: Atom (ConsAnn MonoStackType) MonoStackType -> TempM [Stmt]
writeAtom (IntLit _ i)              = pure $ push 8 (ConstInt $ fromInteger i)
writeAtom (Int8Lit _ i)             = pure $ push 1 (ConstInt8 i)
writeAtom (WordLit _ w)             = pure $ push 8 (ConstWord $ fromIntegral w)
writeAtom (BoolLit _ b)             = pure $ push 1 (ConstBool b)
writeAtom (AtName _ n)              = pure . KCall <$> lookupName n -- TODO: when to do tco?
writeAtom (AtBuiltin ([], _) Drop)  = error "Internal error: Ill-typed drop!"
writeAtom (AtBuiltin ([], _) Dup)   = error "Internal error: Ill-typed dup!"
writeAtom (Dip ([], _) _)           = error "Internal error: Ill-typed dip()!"
writeAtom (AtBuiltin _ IntPlus)     = intOp IntPlusIR
writeAtom (AtBuiltin _ IntMinus)    = intOp IntMinusIR
writeAtom (AtBuiltin _ IntTimes)    = intOp IntTimesIR
writeAtom (AtBuiltin _ IntDiv)      = intOp IntDivIR -- what to do on failure?
writeAtom (AtBuiltin _ IntMod)      = intOp IntModIR
writeAtom (AtBuiltin _ IntXor)      = intOp IntXorIR
writeAtom (AtBuiltin _ IntShiftR)   = intShift WordShiftRIR -- TODO: shr or sar?
writeAtom (AtBuiltin _ IntShiftL)   = intShift WordShiftLIR
writeAtom (AtBuiltin _ IntEq)       = intRel IntEqIR
writeAtom (AtBuiltin _ WordPlus)    = intOp IntPlusIR
writeAtom (AtBuiltin _ WordTimes)   = intOp IntTimesIR
writeAtom (AtBuiltin _ WordXor)     = intOp IntXorIR
writeAtom (AtBuiltin _ WordShiftL)  = intShift WordShiftLIR
writeAtom (AtBuiltin _ WordShiftR)  = intShift WordShiftRIR
writeAtom (AtBuiltin (is, _) Drop)  =
    let sz = size (last is) in
        pure [ dataPointerDec sz ]
writeAtom (AtBuiltin (is, _) Dup)   =
    let sz = size (last is) in
        pure $
             copyBytes 0 (-sz) sz
                ++ [ dataPointerInc sz ] -- move data pointer over sz bytes
writeAtom (If _ as as') = do
    l0 <- newLabel
    l1 <- newLabel
    let ifIR = CJump (Mem 1 (Reg DataPointer)) l0 l1
    asIR <- writeAtoms as
    asIR' <- writeAtoms as'
    l2 <- newLabel
    pure $ dataPointerDec 1 : ifIR : (Labeled l0 : asIR ++ [Jump l2]) ++ (Labeled l1 : asIR') ++ [Labeled l2]
writeAtom (Dip (is, _) as) =
    let sz = size (last is)
    in foldMapA (dipify sz) as
writeAtom (AtBuiltin ([i0, i1], _) Swap) =
    let sz0 = size i0
        sz1 = size i1
    in
        pure $
            copyBytes 0 (-sz0 - sz1) sz0 -- copy i0 to end of the stack
                ++ copyBytes (-sz0 - sz1) (-sz1) sz1 -- copy i1 to where i0 used to be
                ++ copyBytes (-sz0) 0 sz0 -- copy i0 at end of stack to its new place
writeAtom (AtBuiltin _ Swap) = error "Ill-typed swap!"
writeAtom (AtCons ann@(ConsAnn _ tag _) _) =
    pure $ dataPointerInc (padBytes ann) : push 1 (ConstTag tag)

-- | Constructors may need to be padded, this computes the number of bytes of
-- padding
padBytes :: ConsAnn MonoStackType -> Int64
padBytes (ConsAnn sz _ (is, _)) = sz - sizeStack is - 1

-- TODO: need consistent ABI for constructors

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
dipify _ (AtBuiltin _ Swap) = error "Internal error: Ill-typed swap!"
dipify sz (AtBuiltin _ IntTimes) = dipOp sz IntTimesIR
dipify sz (AtBuiltin _ IntPlus)  = dipOp sz IntPlusIR
dipify sz (AtBuiltin _ IntMinus) = dipOp sz IntMinusIR
dipify sz (AtBuiltin _ IntDiv)   = dipOp sz IntDivIR
dipify sz (AtBuiltin _ IntMod)   = dipOp sz IntModIR
dipify sz (AtBuiltin _ IntXor)   = dipOp sz IntXorIR
dipify sz (AtBuiltin _ WordPlus) = dipOp sz IntPlusIR
dipify _ (AtBuiltin ([], _) Dup) = error "Internal error: Ill-typed dup!"
dipify sz (AtBuiltin (is, _) Dup) = do
    let sz' = size (last is) in
        pure $
             copyBytes 0 (-sz) sz -- copy sz bytes over to the end of the stack
                ++ copyBytes (-sz) (-sz - sz') sz' -- copy sz' bytes over (duplicate)
                ++ undefined -- copy sz bytes back
                ++ [ dataPointerInc sz' ] -- move data pointer over sz' bytes

dipOp :: Int64 -> IntBinOp -> TempM [Stmt]
dipOp sz op =
    let shiftNext = dataPointerDec sz
        shiftBack = dataPointerInc sz
        -- copy sz bytes over
        copyBytes' = copyBytes 0 sz sz
    in
        do
            aStmt <- intOp op
            pure (shiftNext : aStmt ++ copyBytes' ++ [shiftBack])

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
