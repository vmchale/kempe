module Data.Copointed ( Copointed (..)
                      ) where

class Copointed p where
    copoint :: p a -> a
