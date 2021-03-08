{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Type ( Label
                          , ArmReg (..)
                          , AbsReg (..)
                          , Arm (..)
                          ) where

import           Control.DeepSeq    (NFData)
import qualified Data.ByteString    as BS
import           Data.Copointed
import           Data.Int           (Int64)
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics       (Generic)
import           Kempe.Asm.Pretty
import           Prettyprinter      (Doc, Pretty (..), brackets, colon, hardline, (<+>))
import           Prettyprinter.Ext  (prettyHex, (<~>))

-- r19-r28 calle-saved
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
            | CRet -- x0
            deriving (Generic, NFData)

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

data Addr reg = Reg reg
              | AddRRPlus reg reg
              deriving (Generic, NFData)

instance (Pretty reg) => Pretty (Addr reg) where
    pretty (Reg reg) = brackets (pretty reg)

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

-- | For reference: https://static.docs.arm.com/100898/0100/the_a64_Instruction_set_100898_0100.pdf
data Arm reg a = Branch { ann :: a, label :: Label } -- like jump
               | BranchLink { ann :: a, label :: Label } -- like @call@
               | AddRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | SubRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MulRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MovRC { ann :: a, dest :: reg, iSrc :: Int64 }
               | SignedDivRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | UnsignedDivRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MovRWord { ann :: a, dest :: reg, wSrc :: Word }
               | MovRR { ann :: a, dest :: reg, src :: reg }
               | AndRR { ann :: a, dest :: reg, inp1 :: reg, inp2 :: reg }
               | Load { ann :: a, dest :: reg, addrSrc :: Addr reg }
               | Store { ann :: a, src :: reg, addrDest :: Addr reg }
               | CmpRR { ann :: a, inp1 :: reg, inp2 :: reg }
               | CSet { ann :: a, dest :: reg, cond :: Cond }
               | Ret { ann :: a }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | LShiftLRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg } -- LShift - logical shift
               | LShiftRRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               deriving (Generic, NFData)

-- | Don't call this on a negative number!
prettyUInt :: (Integral a, Show a) => a -> Doc b
prettyUInt i | i >= 0 = "#" <> prettyHex i
             | otherwise = error "Internal error: prettyUInt called on a negative number!"

instance Pretty reg => Pretty (Arm reg a) where
    pretty (Branch _ l)          = "b" <+> prettyLabel l
    pretty (BranchLink _ l)      = "bl" <+> prettyLabel l
    pretty Ret{}                 = "ret"
    pretty (BSLabel _ b)         = let pl = pretty (decodeUtf8 b) in ".globl" <+> pl <> hardline <> pl <> colon
    pretty (MovRWord _ r c)      = "mov" <+> pretty r <~> prettyUInt c
    pretty (LShiftLRR _ r r0 r1) = "lsl" <+> pretty r <~> pretty r0 <~> pretty r1
    pretty (LShiftRRR _ r r0 r1) = "lsr" <+> pretty r <~> pretty r0 <~> pretty r1

instance Copointed (Arm reg) where
    copoint = ann
