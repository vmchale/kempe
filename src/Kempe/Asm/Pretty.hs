{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Pretty ( i4
                        , prettyLabel
                        ) where

import           Prettyprinter (Doc, indent, pretty)

i4 :: Doc ann -> Doc ann
i4 = indent 4

prettyLabel :: Word -> Doc ann
prettyLabel l = "kmp_" <> pretty l
