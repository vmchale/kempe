module Data.Bifunctor.Ext ( (~<$)
                          ) where

import           Data.Bifunctor (Bifunctor (first))

infixl 4 ~<$

(~<$) :: Bifunctor p => a -> p b c -> p a c
(~<$) x = first (const x)
