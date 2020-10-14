{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error ( Error (..)
                   ) where

import           Data.Semigroup ((<>))
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter  (Pretty (pretty), comma, squotes, (<+>))

-- reject mutually recursive types? idk :p
data Error a = MismatchedTypes a (KempeTy a) (KempeTy a) -- TODO: include atom "expression?"
             | PoorScope a (Name a)
             | MismatchedLengths a (StackType a) (StackType a)

instance Pretty (Error a) where
    pretty (MismatchedTypes _ ty ty')    = "could not match type" <+> squotes (pretty ty) <+> "with type" <+> squotes (pretty ty')
    pretty (PoorScope _ n)               = "name" <+> squotes (pretty n) <+> "not in scope"
    pretty (MismatchedLengths _ st0 st1) = "mismatched type lengths" <+> pretty st0 <> comma <+> pretty st1
