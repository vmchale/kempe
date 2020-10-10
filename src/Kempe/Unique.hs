{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kempe.Unique ( Unique (..)
                    ) where

import           Prettyprinter (Pretty)

newtype Unique = Unique Int
    deriving (Eq, Ord, Pretty)
