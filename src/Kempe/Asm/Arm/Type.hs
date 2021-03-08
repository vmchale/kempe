{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Type ( Label
                          , ArmReg (..)
                          , AbsReg (..)
                          , Arm (..)
                          ) where

import           Control.DeepSeq  (NFData)
import           Data.Copointed
import           Data.Int         (Int64)
import           GHC.Generics     (Generic)
import           Kempe.Asm.Pretty
import           Prettyprinter    (Pretty (..), brackets, (<+>))

-- r19-r28 calle-saved
-- r0-r7 result registers

data AbsReg = DataPointer
            | AllocReg !Int
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

data Cond = Eq
          | Neq
          | Leq
          deriving (Generic, NFData)

instance Pretty Cond where
    pretty Eq  = "EQ"
    pretty Neq = "NE"
    pretty Leq = "LS"

-- see: https://developer.arm.com/documentation/dui0068/b/arm-instruction-reference
data Arm reg a = Branch { ann :: a, label :: Label } -- like "
               | AddRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | SubRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MovRC { ann :: a, dest :: reg, iSrc :: Int64 }
               | MovRR { ann :: a, dest :: reg, src :: reg }
               | AndRR { ann :: a, dest :: reg, inp1 :: reg, inp2 :: reg }
               | Load { ann :: a, dest :: reg, addrSrc :: Addr reg }
               | Store { ann :: a, src :: reg, addrDest :: Addr reg }
               | CmpRR { ann :: a, inp1 :: reg, inp2 :: reg }
               | CSet { ann :: a, dest :: reg, cond :: Cond }
               deriving (Generic, NFData)

instance Pretty reg => Pretty (Arm reg a) where
    pretty (Branch _ l) = "B" <+> prettyLabel l

instance Copointed (Arm reg) where
    copoint = ann
