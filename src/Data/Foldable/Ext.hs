module Data.Foldable.Ext ( foldMapA
                         , foldMapAlternative
                         ) where

import           Control.Applicative (Alternative)
import           Data.Foldable       (asum, fold)

foldMapAlternative :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
foldMapAlternative f xs = asum (f <$> xs)

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse
