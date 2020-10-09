{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error ( Error (..)
                   ) where

import           Kempe.AST
import           Prettyprinter (Pretty (pretty), squotes, (<+>))

-- reject mutually recursive types? idk :p
data Error a = MismatchedTypes a (KempeTy a) (KempeTy a) -- TODO: include atom "expression?"

instance Pretty (Error a) where
    pretty (MismatchedTypes _ ty ty') = "could not match type" <+> squotes (pretty ty) <+> "with type" <+> squotes (pretty ty')
