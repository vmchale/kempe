{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error ( Error (..)
                   ) where

import           Kempe.AST
import           Kempe.Name
import           Prettyprinter (Pretty (pretty), squotes, (<+>))

-- reject mutually recursive types? idk :p
data Error a = MismatchedTypes a (KempeTy a) (KempeTy a)
             | PoorScope a (Name a)

instance Pretty (Error a) where
    pretty (MismatchedTypes _ ty ty') = "could not match type" <+> squotes (pretty ty) <+> "with type" <+> squotes (pretty ty')
    pretty (PoorScope _ n)            = "name" <+> squotes (pretty n) <+> "not in scope"
