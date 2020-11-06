module Data.Foldable.Ext ( foldMapA
                         ) where

import           Data.Foldable (fold)

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse
