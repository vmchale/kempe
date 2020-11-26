module Main (main) where

import           Criterion.Main
import           Foreign.C.Types

foreign import ccall unsafe is_prime :: CInt -> CBool

main :: IO ()
main = do
    defaultMain [ bgroup "is_prime"
                      [ bench "2³² - 5" $ nf is_prime (2 ^ (32 :: Int) - 5)
                      ]
                ]
