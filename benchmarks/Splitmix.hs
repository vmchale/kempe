module Splitmix ( next ) where

import           Data.Bits (shiftR, xor)
import           Data.Word (Word64)

next :: Word64 -> (Word64, Word64)
next seed = (seed', rand)
    where seed' = seed + 0x9e3779b97f4a7c15
          z0 = seed'
          z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
          rand = z2 `xor` (z2 `shiftR` 31)
