module Kempe.Asm.Arm.Opt ( optimizeArm
                         ) where

import           Kempe.Asm.Arm.Type

optimizeArm :: Eq reg => [Arm reg a] -> [Arm reg a]
optimizeArm ((Store l r a):(Load _ r' a'):as)         | r == r' && a == a' = optimizeArm (Store l r a : as)
optimizeArm ((StoreByte l r a):(LoadByte _ r' a'):as) | r == r' && a == a' = optimizeArm (StoreByte l r a : as)
optimizeArm (a:as) = a : optimizeArm as
optimizeArm []     = []
