{-# LANGUAGE OverloadedStrings #-}

module Language.C.AST ( CType (..)
                      , CFunc (..)
                      , prettyHeaders
                      , cSettings
                      ) where

import           Data.Semigroup    ((<>))
import qualified Data.Set          as S
import qualified Data.Text         as T
import           Prettyprinter     (Doc, LayoutOptions (..), PageWidth (..), Pretty (..), tupled, (<+>))
import           Prettyprinter.Ext

cSettings :: LayoutOptions
cSettings = LayoutOptions $ AvailablePerLine 180 0.8

data CHeader = StdBool -- ^ @stdbool.h@
             | StdInt -- ^ @stdint.h@
             deriving (Eq, Ord)

prettyInclude :: CHeader -> Doc ann
prettyInclude StdBool = "#include <stdbool.h>"
prettyInclude StdInt  = "#include <stdint.h>"

data CType = CInt
           | CBool
           | CUInt64
           | CInt8
           | CVoid
           | CVoidPtr
           -- ADTs etc.

data CFunc = CFunc !T.Text [CType] CType

prettyHeaders :: [CFunc] -> Doc ann
prettyHeaders es =
    let hs = foldMap mentionedFunc es
        in prettyLines (fmap prettyInclude (S.toList hs))
            <#> prettyLines (fmap pretty es)

mentioned :: CType -> S.Set CHeader
mentioned CInt     = mempty
mentioned CBool    = S.singleton StdBool
mentioned CUInt64  = S.singleton StdInt
mentioned CVoid    = mempty
mentioned CVoidPtr = mempty
mentioned CInt8    = S.singleton StdInt

mentionedFunc :: CFunc -> S.Set CHeader
mentionedFunc (CFunc _ args ret) = foldMap mentioned (ret : args)

instance Pretty CType where
    pretty CInt     = "int"
    pretty CBool    = "bool"
    pretty CUInt64  = "uint64_t"
    pretty CVoid    = "void"
    pretty CVoidPtr = "void*"
    pretty CInt8    = "int8_t"

instance Pretty CFunc where
    pretty (CFunc fname args retType) = "extern" <+> pretty retType <+> pretty fname <+> tupled (pretty <$> args) <> ";"
