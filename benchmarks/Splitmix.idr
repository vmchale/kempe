module Splitmix

next : Int -> (Int, Int)
next seed = (seed', rand)
    where seed' : Int
          seed' = seed + 0x9e3779b97f4a7c15
          xor : Int -> Int -> Int
          xor = prim__xor_Int
          z1 : Int
          z1 = (seed' `xor` (seed' `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 : Int
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
          rand : Int
          rand = z2 `xor` (z2 `shiftR` 31)
