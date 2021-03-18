{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.X86.Type ( X86 (..)
                          , Addr (..)
                          , AbsReg (..)
                          , X86Reg (..)
                          , Label
                          , prettyAsm
                          , prettyDebugAsm
                          ) where

import           Control.DeepSeq         (NFData)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Copointed
import           Data.Int                (Int64, Int8)
import           Data.Semigroup          ((<>))
import           Data.Text.Encoding      (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Word               (Word8)
import           GHC.Generics            (Generic)
import           Kempe.Asm.Pretty
import           Kempe.Asm.Type
import           Prettyprinter           (Doc, Pretty (pretty), brackets, colon, concatWith, hardline, (<+>))
import           Prettyprinter.Ext

type Label = Word

-- | Used to implement 'MovRRLower' pretty-printer
class As8 reg where
    -- | Return the register for addressing the lower 8 bits given a 64-bit
    -- register
    as8 :: reg -> reg

instance As8 X86Reg where
    as8 R8  = R8b
    as8 R9  = R9b
    as8 R10 = R10b
    as8 R11 = R11b
    as8 R12 = R12b
    as8 R13 = R13b
    as8 R14 = R14b
    as8 R15 = R15b
    as8 Rsi = Sil
    as8 Rdi = Dil
    as8 Rax = AL
    as8 Rdx = DL
    as8 r   = r -- other cases, as8 r8b = r8b, etc.

-- currently just has 64-bit and 8-bit registers
data X86Reg = R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | Rdi -- can I use rsi/rdi??
            | Rsi
            -- -- | BH
            -- -- | BL
            | R8b
            | R9b
            | R10b
            | R11b
            | R12b
            | R13b
            | R14b
            | R15b
            | Sil
            | Dil
            | Rsp
            | Rbp
            | Rbx
            -- cl is reserved in this implementation which it really shouldn't be
            -- rax and rdx (and friends) are reserved for unsigned mult.
            | Rcx
            | CH
            | CL
            -- Rax, Rdx and friends are reserved for integer division (and
            -- unsigned multiplication lol)
            | Rax
            | Rdx
            | AH
            | AL
            | DH
            | DL
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
    pretty Sil  = "sil"
    pretty Dil  = "dil"

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CArg1
            | CArg2
            | CArg3
            | CArg4
            | CArg5
            | CArg6
            | CRet
            | ShiftExponent
            | QuotRes -- quotient register for idiv, rax
            | RemRes -- remainder register for idiv, rdx
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
    pretty QuotRes        = "rax"
    pretty RemRes         = "rdx"

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
               | CallBS { ann :: a, bslLabel :: BSL.ByteString }
               | Ret { ann :: a }
               -- intel-ish syntax; destination first
               | MovRA { ann :: a, rDest :: reg, addrSrc :: Addr reg }
               | MovAR { ann :: a, addrDest :: Addr reg, rSrc :: reg }
               | MovABool { ann :: a, addrDest :: Addr reg, boolSrc :: Word8 }
               | MovRR { ann :: a, rDest :: reg, rSrc :: reg } -- for convenience
               | MovRRLower { ann :: a, rDest :: reg, rSrc :: reg } -- ^ Doesn't correspond 1-1 to an instruction, writes a 64-bit register to an 8-bit register
               | MovRC { ann :: a, rDest :: reg, iSrc :: Int64 }
               | MovRL { ann :: a, rDest :: reg, bsLabel :: BS.ByteString }
               | MovAC { ann :: a, addrDest :: Addr reg, iSrc :: Int64 }
               | MovACi8 { ann :: a, addrDest :: Addr reg, i8Src :: Int8 }
               | MovACTag { ann :: a, addrDest :: Addr reg, tagSrc :: Word8 }
               | MovRCBool { ann :: a, rDest :: reg, boolSrc :: Word8 }
               | MovRCi8 { ann :: a, rDest :: reg, i8Src :: Int8 }
               | MovRCTag { ann :: a, rDest :: reg, tagSrc :: Word8 }
               | MovRWord { ann :: a, rDest :: reg, wSrc :: Word }
               | AddRR { ann :: a, rAdd1 :: reg, rAdd2 :: reg }
               | SubRR { ann :: a, rSub1 :: reg, rSub2 :: reg }
               | XorRR { ann :: a, rXor1 :: reg, rXor2 :: reg }
               | ImulRR { ann :: a, rMul1 :: reg, rMul2 :: reg }
               | AddAC { ann :: a, addrAdd1 :: Addr reg, iAdd2 :: Int64 }
               | AddRC { ann :: a, rAdd1 :: reg, iAdd2 :: Int64 }
               | SubRC { ann :: a, rSub1 :: reg, iSub2 :: Int64 }
               | LShiftLRR { ann :: a, rDest :: reg, rSrc :: reg }
               | LShiftRRR { ann :: a, rDest :: reg, rSrc :: reg }
               | AShiftRRR { ann :: a, rDest :: reg, rSrc :: reg }
               | Label { ann :: a, label :: Label }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | Je { ann :: a, jLabel :: Label }
               | Jne { ann :: a, jLabel :: Label }
               | Jg { ann :: a, jLabel :: Label }
               | Jge { ann :: a, jLabel :: Label }
               | Jl { ann :: a, jLabel :: Label }
               | Jle { ann :: a, jLabel :: Label }
               | CmpAddrReg { ann :: a, addrCmp :: Addr reg, rCmp :: reg }
               | CmpRegReg { ann :: a, rCmp :: reg, rCmp' :: reg } -- for simplicity
               | CmpAddrBool { ann :: a, addrCmp :: Addr reg, bCmp :: Word8 }
               | CmpRegBool { ann :: a, rCmp :: reg, bCmp :: Word8 }
               | IdivR { ann :: a, rDiv :: reg }
               | DivR { ann :: a, rDiv :: reg }
               | Cqo { ann :: a }
               | AndRR { ann :: a, rDest :: reg, rSrc :: reg }
               | OrRR { ann :: a, rDest :: reg, rSrc :: reg }
               | PopcountRR { ann :: a, rDest :: reg, rSrc :: reg }
               | NegR { ann :: a, rSrc :: reg }
               | NasmMacro0 { ann :: a, macroName :: BS.ByteString }
               deriving (Generic, NFData, Functor)

instance Copointed (X86 reg) where
    copoint = ann

instance Pretty reg => Pretty (Addr reg) where
    pretty (Reg r)               = brackets (pretty r)
    pretty (AddrRRPlus r0 r1)    = brackets (pretty r0 <> "+" <> pretty r1)
    pretty (AddrRCPlus r c)      = brackets (pretty r <> "+" <> pretty c)
    pretty (AddrRCMinus r c)     = brackets (pretty r <> "-" <> pretty c)
    pretty (AddrRRScale r0 r1 c) = brackets (pretty r0 <> "+" <> pretty r1 <> "*" <> pretty c)

prettyLive :: (As8 reg, Pretty reg) => X86 reg Liveness -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

-- intel syntax
instance (As8 reg, Pretty reg) => Pretty (X86 reg a) where
    pretty (PushReg _ r)        = i4 ("push" <+> pretty r)
    pretty (PushMem _ a)        = i4 ("push" <+> pretty a)
    pretty (PopMem _ a)         = i4 ("pop qword" <+> pretty a)
    pretty (PopReg _ r)         = i4 ("pop" <+> pretty r)
    pretty (PushConst _ i)      = i4 ("push" <+> pretty i)
    pretty (Jump _ l)           = i4 ("jmp" <+> prettyLabel l)
    pretty (Call _ l)           = i4 ("call" <+> prettyLabel l)
    pretty Ret{}                = i4 "ret"
    pretty (MovRA _ r a)        = i4 ("mov" <+> pretty r <> "," <+> pretty a)
    pretty (MovAR _ a r)        = i4 ("mov" <+> pretty a <> "," <+> pretty r)
    pretty (MovABool _ a b)     = i4 ("mov byte" <+> pretty a <> "," <+> pretty b)
    pretty (MovACi8 _ a i)      = i4 ("mov byte" <+> pretty a <> "," <+> pretty i)
    pretty (MovRCi8 _ r i)      = i4 ("mov byte" <+> pretty r <> "," <+> pretty i)
    pretty (MovRWord _ r w)     = i4 ("mov qword" <+> pretty r <> "," <+> prettyHex w)
    pretty (MovRR _ r0 r1)      = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRRLower _ r0 r1) = i4 ("mov" <+> pretty r0 <> "," <+> pretty (as8 r1))
    pretty (MovRC _ r i)        = i4 ("mov" <+> pretty r <> "," <+> pretty i)
    pretty (MovAC _ a i)        = i4 ("mov qword" <+> pretty a <> "," <+> pretty i)
    pretty (MovRCBool _ r b)    = i4 ("mov" <+> pretty r <> "," <+> pretty b)
    pretty (MovRL _ r bl)       = i4 ("mov" <+> pretty r <> "," <+> pretty (decodeUtf8 bl))
    pretty (AddRR _ r0 r1)      = i4 ("add" <+> pretty r0 <> "," <+> pretty r1)
    pretty (AddAC _ a c)        = i4 ("add" <+> pretty a <> "," <+> pretty c)
    pretty (SubRR _ r0 r1)      = i4 ("sub" <+> pretty r0 <> "," <> pretty r1)
    pretty (ImulRR _ r0 r1)     = i4 ("imul" <+> pretty r0 <> "," <+> pretty r1)
    pretty (XorRR _ r0 r1)      = i4 ("xor" <+> pretty r0 <> "," <+> pretty r1)
    pretty (AddRC _ r0 c)       = i4 ("add" <+> pretty r0 <> "," <+> pretty c)
    pretty (SubRC _ r0 c)       = i4 ("sub" <+> pretty r0 <> "," <+> pretty c)
    pretty (Label _ l)          = prettyLabel l <> colon
    pretty (BSLabel _ b)        = let pl = pretty (decodeUtf8 b) in "global" <+> pl <> hardline <> pl <> colon
    pretty (Je _ l)             = i4 ("je" <+> prettyLabel l)
    pretty (Jl _ l)             = i4 ("jl" <+> prettyLabel l)
    pretty (CmpAddrReg _ a r)   = i4 ("cmp" <+> pretty a <> "," <+> pretty r)
    pretty (CmpRegReg _ r0 r1)  = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (CmpAddrBool _ a b)  = i4 ("cmp byte" <+> pretty a <> "," <+> pretty b)
    pretty (CmpRegBool _ r b)   = i4 ("cmp" <+> pretty r <> "," <+> pretty b)
    pretty (LShiftRRR _ r0 r1)  = i4 ("shr" <+> pretty r0 <> "," <+> pretty r1)
    pretty (LShiftLRR _ r0 r1)  = i4 ("shl" <+> pretty r0 <> "," <+> pretty r1)
    pretty (AShiftRRR _ r0 r1)  = i4 ("sar" <+> pretty r0 <> "," <+> pretty r1)
    pretty (IdivR _ r)          = i4 ("idiv" <+> pretty r)
    pretty (DivR _ r)           = i4 ("div" <+> pretty r)
    pretty Cqo{}                = i4 "cqo"
    pretty (MovACTag _ a t)     = i4 ("mov byte" <+> pretty a <> "," <+> pretty t)
    pretty (AndRR _ r0 r1)      = i4 ("and" <+> pretty r0 <+> pretty r1)
    pretty (OrRR _ r0 r1)       = i4 ("or" <+> pretty r0 <+> pretty r1)
    pretty (PopcountRR _ r0 r1) = i4 ("popcnt" <+> pretty r0 <> "," <+> pretty r1)
    pretty (NegR _ r)           = i4 ("neg" <+> pretty r)
    pretty (Jne _ l)            = i4 ("jne" <+> prettyLabel l)
    pretty (Jg _ l)             = i4 ("jg" <+> prettyLabel l)
    pretty (Jge _ l)            = i4 ("jge" <+> prettyLabel l)
    pretty (Jle _ l)            = i4 ("jle" <+> prettyLabel l)
    pretty (MovRCTag _ r b)     = i4 ("mov" <+> pretty r <> "," <+> pretty b)
    pretty (NasmMacro0 _ b)     = i4 (pretty (decodeUtf8 b))
    pretty (CallBS _ b)         = i4 ("call" <+> pretty (TL.decodeUtf8 b))

prettyAsm :: (As8 reg, Pretty reg) => [X86 reg a] -> Doc ann
prettyAsm = ((prolegomena <#> macros <#> "section .text" <> hardline) <>) . prettyLines . fmap pretty

prettyDebugAsm :: (As8 reg, Pretty reg) => [X86 reg Liveness] -> Doc ann
prettyDebugAsm = concatWith (<#>) . fmap prettyLive

prolegomena :: Doc ann
prolegomena = "BITS 64" <#> "section .bss" <#> "kempe_data: resb 0x8012" -- 32 kb

macros :: Doc ann
macros = prettyLines
    [ calleeSave
    , calleeRestore
    , callerSave
    , callerRestore
    ]

-- rbx, rbp, r12-r15 callee-saved (non-volatile)
-- rest caller-saved (volatile)

-- | Save non-volatile registers
calleeSave :: Doc ann
calleeSave =
    "%macro calleesave 0"
    <#> prettyLines (fmap pretty toPush)
    <#> "%endmacro"
    where toPush = PushReg () <$> [Rbx, Rbp, R12, R13, R14, R15]

calleeRestore :: Doc ann
calleeRestore =
    "%macro calleerestore 0"
    <#> prettyLines (fmap pretty toPop)
    <#> "%endmacro"
    where toPop = PopReg () <$> [R15, R14, R13, R12, Rbp, Rbx]

callerSave :: Doc ann
callerSave =
    "%macro callersave 0"
    <#> prettyLines (fmap pretty toPush)
    <#> "%endmacro"
    where toPush = PushReg () <$> [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]

callerRestore :: Doc ann
callerRestore =
    "%macro callerrestore 0"
    <#> prettyLines (fmap pretty toPop)
    <#> "%endmacro"
    where toPop = PopReg () <$> [R11, R10, R9, R8, Rdi, Rsi, Rdx, Rcx, Rax]
