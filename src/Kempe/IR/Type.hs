{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IR loosely based on Appel book.
module Kempe.IR.Type ( Stmt (..)
                     , Exp (..)
                     , RelBinOp (..)
                     , IntBinOp (..)
                     , BoolBinOp (..)
                     , Label
                     , Temp (..)
                     , WriteSt (..)
                     ) where

import           Control.DeepSeq      (NFData)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Int             (Int64, Int8)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Word            (Word8)
import           GHC.Generics         (Generic)
import           Kempe.AST.Size
import           Prettyprinter        (Doc, Pretty (pretty), braces, brackets, colon, hardline, parens, (<+>))
import           Prettyprinter.Ext

data WriteSt = WriteSt { wlabels :: [Label]
                       , temps   :: [Int]
                       }

type Label = Word

prettyLabel :: Label -> Doc ann
prettyLabel l = "kmp" <> pretty l

data Temp = Temp64 !Int
          | Temp8 !Int
          | DataPointer -- RBP on x86 and x19 on aarch64?
          deriving (Eq, Generic, NFData)

instance Pretty Temp where
    pretty (Temp64 i)  = "t_" <> pretty i
    pretty (Temp8 i)   = "t8_" <> pretty i
    pretty DataPointer = "datapointer"

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
          | CJump Exp Label Label -- ^ If the 'Exp' evaluates to @1@, go to the first label, otherwise go to the second (if-then-else). Used to implement ifs.
          | MJump Exp Label
          | CCall MonoStackType BSL.ByteString
          | KCall Label -- ^ a 'KCall' is a jump to a Kempe procedure
          | WrapKCall ABI MonoStackType BS.ByteString Label
          | MovTemp Temp Exp -- ^ Put @e@ in temp
          | MovMem Exp Int64 Exp -- ^ Store @e2@ at address given by @e1@, with sizing information
          | Ret
          deriving (Generic, NFData)

data Exp = ConstInt Int64
         | ConstInt8 Int8
         | ConstTag Word8 -- ^ Used to distinguish constructors of a sum type
         | ConstWord Word
         | ConstBool Bool
         | Reg Temp -- TODO: size?
         | Mem Int64 Exp -- ^ Fetch from address
         | ExprIntBinOp IntBinOp Exp Exp
         | ExprIntRel RelBinOp Exp Exp
         | BoolBinOp BoolBinOp Exp Exp
         | IntNegIR Exp
         | PopcountIR Exp
         | EqByte Exp Exp
         deriving (Eq, Generic, NFData)
           -- TODO: one for data, one for C ABI

data BoolBinOp = BoolAnd
               | BoolOr
               | BoolXor
               deriving (Eq, Generic, NFData)

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
              deriving (Eq, Generic, NFData)

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
              | WordShiftRIR -- ^ compiles to @shr@ on x86
              | WordShiftLIR
              -- int/word mod are different, see: https://stackoverflow.com/questions/8231882/how-to-implement-the-mod-operator-in-assembly
              | WordModIR
              | WordDivIR
              deriving (Eq, Generic, NFData)

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
