module Main (main) where

-- https://primes.utm.edu/lists/2small/0bit.html

import           Criterion.Main
import           Foreign.C.Types

foreign import ccall unsafe is_prime :: CInt -> CBool

main :: IO ()
main = do
    defaultMain [ bgroup "is_prime"
                      [ bench "2017" $ nf is_prime 2017
                      , bench "9719" $ nf is_prime 9719
                      , bench "4294967296" $ nf is_prime 4294967296
                      , bench "4294967291" $ nf is_prime (2 ^ (32 :: Int) - 5)
                      ]
                ]
