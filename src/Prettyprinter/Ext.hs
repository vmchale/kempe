{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Ext ( (<#>)
                         , prettyHex
                         ) where

import           Numeric       (showHex)
import           Prettyprinter

infixr 6 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

prettyHex :: (Integral a, Show a) => a -> Doc ann
prettyHex x = "0x" <> pretty (showHex x mempty)
