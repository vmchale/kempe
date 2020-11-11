{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.X86.Type ( X86 (..)
                          , Addr (..)
                          , AbsReg (..)
                          , ControlAnn (..)
                          , Liveness (..)
                          , Label
                          ) where

import           Control.DeepSeq    (NFData)
import qualified Data.ByteString    as BS
import           Data.Int           (Int64)
import qualified Data.Set           as S
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word          (Word8)
import           GHC.Generics       (Generic)
import           Prettyprinter      (Doc, Pretty (pretty), brackets, colon, indent, (<+>))

type Label = Word

data Liveness = Liveness { ins :: S.Set AbsReg, out :: S.Set AbsReg }
    deriving (Eq, Generic, NFData)

data ControlAnn = ControlAnn { node     :: !Int
                             , conn     :: [Int]
                             , usesNode :: S.Set AbsReg
                             , defsNode :: S.Set AbsReg
                             } deriving (Generic, NFData)

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CRet -- x0 on aarch64
            deriving (Eq, Ord, Generic, NFData)

instance Pretty AbsReg where
    pretty DataPointer    = "datapointer"
    pretty (AllocReg64 i) = "r" <> pretty i
    pretty (AllocReg8 i)  = "HL" <> pretty i
    pretty CRet           = "rax"

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
               | MovRCBool { ann :: a, rDest :: reg, boolSrc :: Word8 }
               | AddRR { ann :: a, rAdd1 :: reg, rAdd2 :: reg }
               | SubRR { ann :: a, rSub1 :: reg, rSub2 :: reg }
               | MulRR { ann :: a, rMul1 :: reg, rMul2 :: reg }
               | AddRC { ann :: a, rAdd1 :: reg, iAdd2 :: Int64 }
               | SubRC { ann :: a, rSub1 :: reg, iSub2 :: Int64 }
               | Label { ann :: a, label :: Label }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | Je { ann :: a, jLabel :: Label }
               | CmpAddrReg { ann :: a, addrCmp :: Addr reg, rCmp :: reg }
               | CmpRegReg { ann :: a, rCmp :: reg, rCmp' :: reg } -- for simplicity
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

-- intel syntax
instance Pretty reg => Pretty (X86 reg a) where
    pretty (PushReg _ r)       = i4 ("push" <+> pretty r)
    pretty (PushMem _ a)       = i4 ("push" <+> pretty a)
    pretty (PopMem _ a)        = i4 ("pop" <+> pretty a)
    pretty (PushConst _ i)     = i4 ("push" <+> pretty i)
    pretty (Jump _ l)          = i4 ("jump" <+> prettyLabel l)
    pretty (Call _ l)          = i4 ("call" <+> prettyLabel l)
    pretty Ret{}               = i4 "ret"
    pretty (MovRA _ r a)       = i4 ("mov" <+> pretty r <> "," <+> pretty a)
    pretty (MovAR _ a r)       = i4 ("mov" <+> pretty a <> "," <+> pretty r)
    pretty (MovABool _ a b)    = i4 ("mov byte ptr" <+> pretty a <> "," <+> pretty b) -- TODO: indicate it's one byte?
    pretty (MovRR _ r0 r1)     = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRC _ r i)       = i4 ("mov" <+> pretty r <> "," <+> pretty i)
    pretty (MovRCBool _ r b)   = i4 ("mov" <+> pretty r <> "," <+> pretty b)
    pretty (AddRR _ r0 r1)     = i4 ("add" <+> pretty r0 <> "," <> pretty r1)
    pretty (SubRR _ r0 r1)     = i4 ("sub" <+> pretty r0 <> "," <> pretty r1)
    pretty (MulRR _ r0 r1)     = i4 ("imul" <+> pretty r0 <> "," <+> pretty r1)
    pretty (AddRC _ r0 c)      = i4 ("add" <+> pretty r0 <> "," <+> pretty c)
    pretty (SubRC _ r0 c)      = i4 ("add" <+> pretty r0 <> "," <+> pretty c)
    pretty (Label _ l)         = prettyLabel l <> colon
    pretty (BSLabel _ b)       = pretty (decodeUtf8 b) <> colon
    pretty (Je _ l)            = i4 ("je" <+> prettyLabel l)
    pretty (CmpAddrReg _ a r)  = i4 ("cmp" <+> pretty a <> "," <+> pretty r)
    pretty (CmpRegReg _ r0 r1) = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
