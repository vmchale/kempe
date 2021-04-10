{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Pretty ( i4
                        , prettyLabel
                        , prettyBS
                        ) where

import qualified Data.ByteString           as BS
import           Data.Semigroup            ((<>))
import           Data.Text.Encoding        (encodeUtf8)
import           Prettyprinter             (Doc, Pretty, indent, layoutCompact, pretty)
import           Prettyprinter.Render.Text (renderStrict)

prettyBS :: Pretty a => a -> BS.ByteString
prettyBS = encodeUtf8 . renderStrict . layoutCompact . pretty

i4 :: Doc ann -> Doc ann
i4 = indent 4

prettyLabel :: Word -> Doc ann
prettyLabel l = "kmp_" <> pretty l
