{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.X86.Type ( X86 (..)
                          , Addr (..)
                          , AbsReg (..)
                          , X86Reg (..)
                          , ControlAnn (..)
                          , Liveness (..)
                          , Label
                          , prettyAsm
                          ) where

import           Control.DeepSeq    (NFData)
import qualified Data.ByteString    as BS
import           Data.Foldable      (toList)
import           Data.Int           (Int64, Int8)
import qualified Data.Set           as S
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word          (Word8)
import           GHC.Generics       (Generic)
import           Numeric            (showHex)
import           Prettyprinter      (Doc, Pretty (pretty), braces, brackets, colon, concatWith, hardline, indent, punctuate, (<+>))

type Label = Word

data Liveness = Liveness { ins :: S.Set AbsReg, out :: S.Set AbsReg }
    deriving (Eq, Generic, NFData)

instance Pretty Liveness where
    pretty (Liveness is os) = braces (pp is <+> ";" <+> pp os)
        where pp = mconcat . punctuate "," . fmap pretty . toList

data ControlAnn = ControlAnn { node     :: !Int
                             , conn     :: [Int]
                             , usesNode :: S.Set AbsReg
                             , defsNode :: S.Set AbsReg
                             } deriving (Generic, NFData)

-- currently just has 64-bit and 8-bit registers
data X86Reg = Rax
            | Rdx
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | AH
            | AL
            -- -- | BH
            -- -- | BL
            | DH
            | DL
            | R8b
            | R9b
            | R10b
            | R11b
            | R12b
            | R13b
            | R14b
            | R15b
            | Rsp
            | Rbp
            | Rbx
            | Rdi
            | Rsi
            -- cl is reserved in this implementation which it really shouldn't be
            -- rax and rdx (and friends) are reserved for unsigned mult.
            | Rcx
            | CH
            | CL
            deriving (Eq, Ord, Enum, Bounded, Generic, NFData)

instance Pretty X86Reg where
    pretty Rax  = "rax"
    pretty Rcx  = "rcx"
    pretty Rdx  = "rdx"
    pretty Rsp  = "rsp"
    pretty Rbp  = "rbp"
    pretty AH   = "ah"
    pretty AL   = "al"
    pretty CH   = "ch"
    pretty CL   = "cl"
    pretty DH   = "dh"
    pretty DL   = "dl"
    pretty Rbx  = "rbx"
    pretty R8   = "r8"
    pretty R9   = "r9"
    pretty R10  = "r10"
    pretty R11  = "r11"
    pretty R12  = "r12"
    pretty R13  = "r13"
    pretty R14  = "r14"
    pretty R15  = "r15"
    pretty R8b  = "r8b"
    pretty R9b  = "r9b"
    pretty R10b = "r10b"
    pretty R11b = "r11b"
    pretty R12b = "r12b"
    pretty R13b = "r13b"
    pretty R14b = "r14b"
    pretty R15b = "r15b"
    pretty Rsi  = "rsi"
    pretty Rdi  = "rdi"

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CArg1
            | CArg2
            | CArg3
            | CArg4
            | CArg5
            | CArg6
            | CRet -- x0 on aarch64
            | ShiftExponent
            deriving (Eq, Ord, Generic, NFData)

instance Pretty AbsReg where
    pretty DataPointer    = "datapointer"
    pretty (AllocReg64 i) = "r" <> pretty i
    pretty (AllocReg8 i)  = "HL" <> pretty i
    pretty CRet           = "rax"
    pretty CArg1          = "rdi"
    pretty CArg2          = "rsi"
    pretty CArg3          = "rdx"
    pretty CArg4          = "rcx"
    pretty CArg5          = "r8"
    pretty CArg6          = "r9"
    pretty ShiftExponent  = "cl"

-- [ebx+ecx*4h-20h]
data Addr reg = Reg reg
              | AddrRRPlus reg reg
              | AddrRCPlus reg Int64
              | AddrRCMinus reg Int64
              | AddrRRScale reg reg Int64
              deriving (Generic, NFData)

-- TODO: sanity-check pass to make sure no Reg8's are in e.g. MovRCBool

-- parametric in @reg@; we do register allocation second
data X86 reg a = PushReg { ann :: a, rSrc :: reg }
               | PushMem { ann :: a, addr :: Addr reg }
               | PopMem { ann :: a, addr :: Addr reg }
               | PopReg { ann :: a, reg :: reg }
               | PushConst { ann :: a, iSrc :: Int64 }
               | Jump { ann :: a, label :: Label }
               | Call { ann :: a, label :: Label }
               | Ret { ann :: a }
               -- intel-ish syntax; destination first
               | MovRA { ann :: a, rDest :: reg, addrSrc :: Addr reg }
               | MovAR { ann :: a, addrDest :: Addr reg, rSrc :: reg }
               | MovABool { ann :: a, addrDest :: Addr reg, boolSrc :: Word8 }
               | MovRR { ann :: a, rDest :: reg, rSrc :: reg } -- for convenience
               | MovRC { ann :: a, rDest :: reg, iSrc :: Int64 }
               | MovRL { ann :: a, rDest :: reg, bsLabel :: BS.ByteString }
               | MovAC { ann :: a, addrDest :: Addr reg, iSrc :: Int64 }
               | MovACi8 { ann :: a, addrDest :: Addr reg, i8Src :: Int8 }
               | MovRCBool { ann :: a, rDest :: reg, boolSrc :: Word8 }
               | MovRCi8 { ann :: a, rDest :: reg, i8Src :: Int8 }
               | MovRWord { ann :: a, rDest :: reg, wSrc :: Word }
               | AddRR { ann :: a, rAdd1 :: reg, rAdd2 :: reg }
               | SubRR { ann :: a, rSub1 :: reg, rSub2 :: reg }
               | XorRR { ann :: a, rXor1 :: reg, rXor2 :: reg }
               | ImulRR { ann :: a, rMul1 :: reg, rMul2 :: reg }
               | AddAC { ann :: a, addrAdd1 :: Addr reg, iAdd2 :: Int64 }
               | AddRC { ann :: a, rAdd1 :: reg, iAdd2 :: Int64 }
               | SubRC { ann :: a, rSub1 :: reg, iSub2 :: Int64 }
               | ShiftLRR { ann :: a, rDest :: reg, rSrc :: reg }
               | ShiftRRR { ann :: a, rDest :: reg, rSrc :: reg }
               | Label { ann :: a, label :: Label }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | Je { ann :: a, jLabel :: Label }
               | CmpAddrReg { ann :: a, addrCmp :: Addr reg, rCmp :: reg }
               | CmpRegReg { ann :: a, rCmp :: reg, rCmp' :: reg } -- for simplicity
               | CmpAddrBool { ann :: a, addrCmp :: Addr reg, bCmp :: Word8 }
               deriving (Generic, NFData, Functor)

i4 :: Doc ann -> Doc ann
i4 = indent 4

instance Pretty reg => Pretty (Addr reg) where
    pretty (Reg r)               = brackets (pretty r)
    pretty (AddrRRPlus r0 r1)    = brackets (pretty r0 <> "+" <> pretty r1)
    pretty (AddrRCPlus r c)      = brackets (pretty r <> "+" <> pretty c)
    pretty (AddrRCMinus r c)     = brackets (pretty r <> "-" <> pretty c)
    pretty (AddrRRScale r0 r1 c) = brackets (pretty r0 <> "+" <> pretty r1 <> "*" <> pretty c)

prettyLabel :: Label -> Doc ann
prettyLabel l = "kmp_" <> pretty l

prettyHex :: (Integral a, Show a) => a -> Doc ann
prettyHex x = "0x" <> pretty (showHex x mempty)

-- intel syntax
instance Pretty reg => Pretty (X86 reg a) where
    pretty (PushReg _ r)       = i4 ("push" <+> pretty r)
    pretty (PushMem _ a)       = i4 ("push" <+> pretty a)
    pretty (PopMem _ a)        = i4 ("pop qword" <+> pretty a)
    pretty (PopReg _ r)        = i4 ("pop" <+> pretty r)
    pretty (PushConst _ i)     = i4 ("push" <+> pretty i)
    pretty (Jump _ l)          = i4 ("jmp" <+> prettyLabel l)
    pretty (Call _ l)          = i4 ("call" <+> prettyLabel l)
    pretty Ret{}               = i4 "ret"
    pretty (MovRA _ r a)       = i4 ("mov" <+> pretty r <> "," <+> pretty a)
    pretty (MovAR _ a r)       = i4 ("mov" <+> pretty a <> "," <+> pretty r)
    pretty (MovABool _ a b)    = i4 ("mov byte" <+> pretty a <> "," <+> pretty b)
    pretty (MovACi8 _ a i)     = i4 ("mov byte" <+> pretty a <> "," <+> pretty i)
    pretty (MovRCi8 _ r i)     = i4 ("mov byte" <+> pretty r <> "," <+> pretty i)
    pretty (MovRWord _ r w)    = i4 ("mov qword" <+> pretty r <> "," <+> prettyHex w)
    pretty (MovRR _ r0 r1)     = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRC _ r i)       = i4 ("mov" <+> pretty r <> "," <+> pretty i)
    pretty (MovAC _ a i)       = i4 ("mov qword" <+> pretty a <> "," <+> pretty i)
    pretty (MovRCBool _ r b)   = i4 ("mov" <+> pretty r <> "," <+> pretty b)
    pretty (MovRL _ r bl)      = i4 ("mov" <+> pretty r <> "," <+> pretty (decodeUtf8 bl))
    pretty (AddRR _ r0 r1)     = i4 ("add" <+> pretty r0 <> "," <> pretty r1)
    pretty (AddAC _ a c)       = i4 ("add" <+> pretty a <> "," <+> pretty c)
    pretty (SubRR _ r0 r1)     = i4 ("sub" <+> pretty r0 <> "," <> pretty r1)
    pretty (ImulRR _ r0 r1)    = i4 ("imul" <+> pretty r0 <> "," <+> pretty r1)
    pretty (XorRR _ r0 r1)     = i4 ("xor" <+> pretty r0 <> "," <+> pretty r1)
    pretty (AddRC _ r0 c)      = i4 ("add" <+> pretty r0 <> "," <+> pretty c)
    pretty (SubRC _ r0 c)      = i4 ("sub" <+> pretty r0 <> "," <+> pretty c)
    pretty (Label _ l)         = prettyLabel l <> colon
    pretty (BSLabel _ b)       = let pl = pretty (decodeUtf8 b) in "global" <+> pl <> hardline <> pl <> colon
    pretty (Je _ l)            = i4 ("je" <+> prettyLabel l)
    pretty (CmpAddrReg _ a r)  = i4 ("cmp" <+> pretty a <> "," <+> pretty r)
    pretty (CmpRegReg _ r0 r1) = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (CmpAddrBool _ a b) = i4 ("cmp byte" <+> pretty a <> "," <+> pretty b)
    pretty (ShiftRRR _ r0 r1)  = i4 ("shr" <+> pretty r0 <> "," <+> pretty r1)
    pretty (ShiftLRR _ r0 r1)  = i4 ("shl" <+> pretty r0 <> "," <+> pretty r1)

prettyAsm :: Pretty reg => [X86 reg a] -> Doc ann
prettyAsm = ((prolegomena <> hardline <> "section .text" <> hardline) <>) . concatWith (\x y -> x <> hardline <> y) . fmap pretty

prolegomena :: Doc ann
prolegomena = "section .bss" <> hardline <> "kempe_data: resb 0x800000"
