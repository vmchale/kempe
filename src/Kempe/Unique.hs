{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kempe.Unique ( Unique (..)
                    ) where

import           Prettyprinter (Pretty)

newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord, Pretty)
