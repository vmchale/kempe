{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Debug ( prettyBind
                           , prettyBound
                           , (<#*>)
                           ) where

import           Data.Semigroup ((<>))
import           Prettyprinter  (Doc, Pretty (pretty), hardline, indent, (<+>))


(<#*>) :: Doc a -> Doc a -> Doc a
(<#*>) x y = x <> hardline <> indent 2 y

prettyBind :: (Pretty c, Pretty b) => (c, b) -> Doc a
prettyBind (i, j) = pretty i <+> "→" <+> pretty j

prettyBound :: (Pretty a, Pretty c) => (a, c) -> Doc b
prettyBound (i, e) = pretty i <+> "←" <#*> pretty e
