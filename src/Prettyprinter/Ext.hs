{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Ext ( (<#>)
                         , (<##>)
                         , (<~>)
                         , prettyHex
                         , prettyLines
                         , sepDecls
                         ) where

import           Numeric       (showHex)
import           Prettyprinter

infixr 6 <#>
infixr 6 <##>
infixr 6 <~>

--- ₀₁₂₃₄₅₆₇₈₉

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

(<##>) :: Doc a -> Doc a -> Doc a
(<##>) x y = x <> hardline <> hardline <> y

(<~>) :: Doc a -> Doc a -> Doc a
(<~>) x y = x <> ", " <> y

prettyHex :: (Integral a, Show a) => a -> Doc ann
prettyHex x = "0x" <> pretty (showHex x mempty)

prettyLines :: [Doc ann] -> Doc ann
prettyLines = concatWith (<#>)

sepDecls :: [Doc ann] -> Doc ann
sepDecls = concatWith (<##>)
