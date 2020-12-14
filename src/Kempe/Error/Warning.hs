{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error.Warning ( Warning (..)
                           ) where

import           Control.Applicative ((<|>))
import           Control.Exception   (Exception)
import           Data.Foldable       (toList)
import           Data.Foldable.Ext
import           Data.List           (group, sort)
import           Data.Maybe          (mapMaybe)
import           Data.Semigroup      ((<>))
import           Data.Typeable       (Typeable)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter       (Pretty (pretty))

data Warning a = NameClash a (Name a)

instance Pretty a => Pretty (Warning a) where
    pretty (NameClash l x) = pretty l <> " '" <> pretty x <> "' is defined more than once."

instance (Pretty a) => Show (Warning a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Warning a)
