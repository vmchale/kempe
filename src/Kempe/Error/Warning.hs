{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error.Warning ( Warning (..)
                           ) where

import           Control.Exception (Exception)
import           Data.Semigroup    ((<>))
import           Data.Typeable     (Typeable)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter     (Pretty (pretty), squotes, (<+>))

data Warning a = NameClash a (Name a)
               | DoubleDip a (Atom a a) (Atom a a)
               | SwapBinary a (Atom a a) (Atom a a)
               | DoubleSwap a
               | DipAssoc a (Atom a a)
               | Identity a (Atom a a)

instance Pretty a => Pretty (Warning a) where
    pretty (NameClash l x)     = pretty l <> " '" <> pretty x <> "' is defined more than once."
    pretty (DoubleDip l a a')  = pretty l <+> pretty a <+> pretty a' <+> "could be written as a single dip()"
    pretty (SwapBinary l a a') = pretty l <+> squotes ("swap" <+> pretty a) <+> "is" <+> pretty a'
    pretty (DoubleSwap l)      = pretty l <+> "double swap"
    pretty (DipAssoc l a)      = pretty l <+> "dip(" <> pretty a <> ")" <+> pretty a <+> "is equivalent to" <+> pretty a <+> pretty a <+> "by associativity"
    pretty (Identity l a)      = pretty l <+> squotes ("dup" <+> pretty a) <+> "is identity"

instance (Pretty a) => Show (Warning a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Warning a)
