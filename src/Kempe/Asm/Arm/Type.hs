{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Type ( Label
                          , ArmReg (..)
                          , AbsReg (..)
                          , Arm (..)
                          , Cond (..)
                          , Addr (..)
                          , prettyAsm
                          , prettyDebugAsm
                          ) where

import           Control.DeepSeq    (NFData)
import qualified Data.ByteString    as BS
import           Data.Copointed
import           Data.Int           (Int64)
import           Data.Semigroup     ((<>))
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics       (Generic)
import           Kempe.Asm.Pretty
import           Kempe.Asm.Type
import           Prettyprinter      (Doc, Pretty (..), brackets, colon, concatWith, hardline, (<+>))
import           Prettyprinter.Ext  (prettyHex, prettyLines, (<#>), (<~>))

-- | Sort of silly class that prints the 32-bit equivalent of a register.
class As32 reg where
    as32b :: reg -> Doc ann

-- r0-r7 result registers

data AbsReg = DataPointer
            | AllocReg !Int
            | CArg0 -- x0
            | CArg1
            | CArg2
            | CArg3
            | CArg4
            | CArg5
            | CArg6
            | CArg7 -- x7
            deriving (Generic, NFData)

instance Pretty AbsReg where
    pretty DataPointer  = "datapointer"
    pretty (AllocReg i) = "Abs" <> pretty i -- ₀₁₂₃₄₅₆₇₈₉
    pretty CArg0        = "X0"
    pretty CArg1        = "X1"
    pretty CArg2        = "X2"
    pretty CArg3        = "X3"
    pretty CArg4        = "X4"
    pretty CArg5        = "X5"
    pretty CArg6        = "X6"
    pretty CArg7        = "X7"

instance As32 AbsReg where
    as32b = pretty

type Label = Word

data ArmReg = X0
            | X1
            | X2
            | X3
            | X4
            | X5
            | X6
            | X7
            | X8
            | X9
            | X10
            | X11
            | X12
            | X13
            | X14
            | X15
            | X16
            | X17
            | X18
            | X19
            | X20
            | X21
            | X22
            | X23
            | X24
            | X25
            | X26
            | X27
            | X28
            | X29
            | X30
            | SP -- ^ Don't use this
            deriving (Enum, Eq, Ord, Generic, NFData)

instance Pretty ArmReg where
    pretty X0  = "x0"
    pretty X1  = "x1"
    pretty X2  = "x2"
    pretty X3  = "x3"
    pretty X4  = "x4"
    pretty X5  = "x5"
    pretty X6  = "x6"
    pretty X7  = "x7"
    pretty X8  = "x8"
    pretty X9  = "x9"
    pretty X10 = "x10"
    pretty X11 = "x11"
    pretty X12 = "x12"
    pretty X13 = "x13"
    pretty X14 = "x14"
    pretty X15 = "x15"
    pretty X16 = "x16"
    pretty X17 = "x17"
    pretty X18 = "x18"
    pretty X19 = "x19"
    pretty X20 = "x20"
    pretty X21 = "x21"
    pretty X22 = "x22"
    pretty X23 = "x23"
    pretty X24 = "x24"
    pretty X25 = "x25"
    pretty X26 = "x26"
    pretty X27 = "x27"
    pretty X28 = "x28"
    pretty X29 = "x29"
    pretty X30 = "x30"
    pretty SP  = "sp"

instance As32 ArmReg where
    as32b X0  = "w0"
    as32b X1  = "w1"
    as32b X2  = "w2"
    as32b X3  = "w3"
    as32b X4  = "w4"
    as32b X5  = "w5"
    as32b X6  = "w6"
    as32b X7  = "w7"
    as32b X8  = "w8"
    as32b X9  = "w9"
    as32b X10 = "w10"
    as32b X11 = "w11"
    as32b X12 = "w12"
    as32b X13 = "w13"
    as32b X14 = "w14"
    as32b X15 = "w15"
    as32b X16 = "w16"
    as32b X17 = "w17"
    as32b X18 = "w18"
    as32b X19 = "w19"
    as32b X20 = "w20"
    as32b X21 = "w21"
    as32b X22 = "w22"
    as32b X23 = "w23"
    as32b X24 = "w24"
    as32b X25 = "w25"
    as32b X26 = "w26"
    as32b X27 = "w27"
    as32b X28 = "w28"
    as32b X29 = "w29"
    as32b X30 = "w30"
    as32b SP  = error "Internal error: as32b sp should not happen!!"

data Addr reg = Reg reg
              | AddRRPlus reg reg
              | AddRCPlus reg Int64
              deriving (Generic, NFData)

instance (Pretty reg) => Pretty (Addr reg) where
    pretty (Reg reg)         = brackets (pretty reg)
    pretty (AddRRPlus r0 r1) = brackets (pretty r0 <~> pretty r1)
    pretty (AddRCPlus r i)   = brackets (pretty r <~> prettyInt i)

-- | See: https://developer.arm.com/documentation/dui0068/b/arm-instruction-reference/conditional-execution?lang=en
data Cond = Eq
          | Neq
          | UnsignedLeq
          | UnsignedGeq
          | UnsignedLt
          | Geq
          | Lt
          | Gt
          | Leq
          deriving (Generic, NFData)

instance Pretty Cond where
    pretty Eq          = "EQ"
    pretty Neq         = "NE"
    pretty UnsignedLeq = "LS"
    pretty Geq         = "GE"
    pretty Lt          = "LT"
    pretty Gt          = "GT"
    pretty Leq         = "LE"
    pretty UnsignedLt  = "LO"

-- | For reference: https://static.docs.arm.com/100898/0100/the_a64_Instruction_set_100898_0100.pdf
data Arm reg a = Branch { ann :: a, label :: Label } -- like jump
               | BranchLink { ann :: a, label :: Label } -- like @call@
               | BranchCond { ann :: a, label :: Label, cond :: Cond }
               | BranchZero { ann :: a, condReg :: reg, label :: Label }
               | AddRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | AddRC { ann :: a, res :: reg, inp1 :: reg, int :: Int64 }
               | SubRC { ann :: a, res :: reg, inp1 :: reg, int :: Int64 }
               | SubRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MulRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MulSubRRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg, inp3 :: reg }
               | MovRC { ann :: a, dest :: reg, iSrc :: Int64 }
               | SignedDivRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | UnsignedDivRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MovRWord { ann :: a, dest :: reg, wSrc :: Word }
               | MovRR { ann :: a, dest :: reg, src :: reg }
               | AndRR { ann :: a, dest :: reg, inp1 :: reg, inp2 :: reg }
               | XorRR { ann :: a, dest :: reg, inp1 :: reg, inp2 :: reg }
               | Load { ann :: a, dest :: reg, addrSrc :: Addr reg }
               | LoadByte { ann :: a, dest :: reg, addrSrc :: Addr reg }
               | LoadLabel { ann :: a, dest :: reg, srcLabel :: BS.ByteString }
               | Store { ann :: a, src :: reg, addrDest :: Addr reg }
               | StoreByte { ann :: a, src :: reg, addrDest :: Addr reg } -- ^ @strb@ in Aarch64 assembly, "store byte"
               | CmpRR { ann :: a, inp1 :: reg, inp2 :: reg }
               | CSet { ann :: a, dest :: reg, cond :: Cond }
               | Ret { ann :: a }
               | Label { ann :: a, label :: Label }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | LShiftLRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg } -- LShift - logical shift
               | LShiftRRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | GnuMacro { ann :: a, macroName :: BS.ByteString }
               | Neg { ann :: a, dest :: reg, src :: reg }
               deriving (Functor, Generic, NFData)

-- | Don't call this on a negative number!
prettyUInt :: (Integral a, Show a) => a -> Doc b
prettyUInt i = "#" <> prettyHex i

prettyInt :: (Pretty a) => a -> Doc b
prettyInt = ("#" <>) . pretty

instance (Pretty reg, As32 reg) => Pretty (Arm reg a) where
    pretty (Branch _ l)              = i4 ("b" <+> prettyLabel l)
    pretty (BranchLink _ l)          = i4 ("bl" <+> prettyLabel l)
    pretty (BranchCond _ l c)        = i4 ("b." <> pretty c <+> prettyLabel l)
    pretty (BranchZero _ r l)        = i4 ("cbz" <+> pretty r <~> prettyLabel l)
    pretty Ret{}                     = i4 "ret"
    pretty (BSLabel _ b)             = let pl = pretty (decodeUtf8 b) in ".globl" <+> pl <> hardline <> pl <> colon
    pretty (MovRWord _ r c)          = i4 ("mov" <+> pretty r <~> prettyUInt c)
    pretty (LShiftLRR _ r r0 r1)     = i4 ("lsl" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (LShiftRRR _ r r0 r1)     = i4 ("lsr" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (AddRR _ r r0 r1)         = i4 ("add" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (SubRR _ r r0 r1)         = i4 ("sub" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (MulRR _ r r0 r1)         = i4 ("mul" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (MulSubRRR _ r r0 r1 r2)  = i4 ("msub" <+> pretty r <~> pretty r0 <~> pretty r1 <~> pretty r2)
    pretty (SignedDivRR _ r r0 r1)   = i4 ("sdiv" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (UnsignedDivRR _ r r0 r1) = i4 ("udiv" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (Load _ r a)              = i4 ("ldr" <+> pretty r <~> pretty a)
    pretty (LoadByte _ r a)          = i4 ("ldrb" <+> as32b r <~> pretty a)
    pretty (LoadLabel _ r l)         = i4 ("ldr" <+> pretty r <~> pretty (decodeUtf8 l))
    pretty (Store _ r a)             = i4 ("str" <+> pretty r <~> pretty a)
    pretty (StoreByte _ r a)         = i4 ("strb" <+> as32b r <~> pretty a)
    pretty (MovRR _ r0 r1)           = i4 ("mov" <+> pretty r0 <~> pretty r1)
    pretty (AndRR _ r r0 r1)         = i4 ("and" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (XorRR _ r r0 r1)         = i4 ("eor" <+> pretty r <~> pretty r0 <~> pretty r1)
    pretty (CSet _ r c)              = i4 ("cset" <+> pretty r <~> pretty c)
    pretty (MovRC _ r i)             = i4 ("mov" <+> pretty r <~> prettyInt i)
    pretty (CmpRR _ r0 r1)           = i4 ("cmp" <+> pretty r0 <~> pretty r1)
    pretty (Label _ l)               = prettyLabel l <> colon
    pretty (GnuMacro _ b)            = i4 (pretty (decodeUtf8 b))
    pretty (AddRC _ r r0 i)          = i4 ("add" <+> pretty r <~> pretty r0 <~> "#" <> pretty i)
    pretty (SubRC _ r r0 i)          = i4 ("sub" <+> pretty r <~> pretty r0 <~> "#" <> pretty i)
    pretty (Neg _ r0 r1)             = i4 ("neg" <+> pretty r0 <~> pretty r1)

instance Copointed (Arm reg) where
    copoint = ann

prettyAsm :: (Pretty reg, As32 reg) => [Arm reg a] -> Doc ann
prettyAsm = (<> hardline) . ((prolegomena <#> macros <#> ".text" <> hardline) <>) . prettyLines . fmap pretty

-- http://www.mathcs.emory.edu/~cheung/Courses/255/Syl-ARM/7-ARM/array-define.html
prolegomena :: Doc ann
prolegomena = ".data" <#> "kempe_data: .skip 32768" -- 32kb

macros :: Doc ann
macros = prettyLines
    [ calleeSave
    , calleeRestore
    , callerSave
    , callerRestore
    ]

-- see:
-- https://community.arm.com/developer/ip-products/processors/b/processors-ip-blog/posts/using-the-stack-in-aarch64-implementing-push-and-pop

calleeSave :: Doc ann
calleeSave =
    ".macro calleesave"
    <#> i4 "sub sp, sp, #(8 * 10)" -- allocate space on stack
    <#> prettyLines (fmap pretty stores)
    <#> ".endm"
    where toPush = [X19 .. X28]
          stores = zipWith (\r o -> Store () r (AddRCPlus SP (8*o))) toPush [0..]

calleeRestore :: Doc ann
calleeRestore =
    ".macro calleerestore"
    <#> prettyLines (fmap pretty loads)
    <#> i4 "add sp, sp, #(8 * 10)" -- free stack space
    <#> ".endm"
    where toPop = [X19 .. X28]
          loads = zipWith (\r o -> Load () r (AddRCPlus SP (8*o))) toPop [0..]

callerSave :: Doc ann
callerSave =
    ".macro callersave"
    <#> i4 "sub sp, sp, #(8 * 8)" -- only 7 stored, but arm stack is 16-byte aligned
    <#> prettyLines (fmap pretty stores)
    <#> ".endm"
    where toPush = [X9 .. X15]
          stores = zipWith (\r o -> Store () r (AddRCPlus SP (8*o))) toPush [0..]

callerRestore :: Doc ann
callerRestore =
    ".macro callerrestore"
    <#> prettyLines (fmap pretty loads)
    <#> i4 "add sp, sp, #(8 * 8)"
    <#> ".endm"
    where toPop = [X9 .. X15]
          loads = zipWith (\r o -> Load () r (AddRCPlus SP (8*o))) toPop [0..]

prettyLive :: (As32 reg, Pretty reg) => Arm reg Liveness -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyDebugAsm :: (As32 reg, Pretty reg) => [Arm reg Liveness] -> Doc ann
prettyDebugAsm = concatWith (<#>) . fmap prettyLive
