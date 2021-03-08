{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Arm.Type ( Label
                          , ArmReg (..)
                          , Arm (..)
                          ) where

import           Data.Copointed
import           Data.Int         (Int64)
import           Kempe.Asm.Pretty
import           Prettyprinter    (Pretty (..), brackets, (<+>))

-- r19-r28 calle-saved
-- r0-r7 result registers

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

newtype Addr reg = Reg reg

instance (Pretty reg) => Pretty (Addr reg) where
    pretty (Reg reg) = brackets (pretty reg)

data Cond = Eq
          | Neq
          | Leq

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

instance Pretty reg => Pretty (Arm reg a) where
    pretty (Branch _ l) = "B" <+> prettyLabel l

instance Copointed (Arm reg) where
    copoint = ann
