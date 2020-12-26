{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error.Warning ( Warning (..)
                           ) where

import           Control.Exception (Exception)
import           Data.Semigroup    ((<>))
import           Data.Typeable     (Typeable)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter     (Pretty (pretty), (<+>))

data Warning a = NameClash a (Name a)
               | DoubleDip a (Atom a a) (Atom a a)

instance Pretty a => Pretty (Warning a) where
    pretty (NameClash l x)    = pretty l <> " '" <> pretty x <> "' is defined more than once."
    pretty (DoubleDip l a a') = pretty l <+> pretty a <+> pretty a' <+> "could be written as a single dip()"

instance (Pretty a) => Show (Warning a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Warning a)
