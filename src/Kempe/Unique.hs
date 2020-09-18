module Kempe.Unique ( Unique (..)
                    ) where

newtype Unique = Unique Int
    deriving (Eq, Ord)
