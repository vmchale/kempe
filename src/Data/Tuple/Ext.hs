module Data.Tuple.Ext ( fst3
                      , snd3
                      , thd3
                      , third3
                      ) where

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

third3 f (x, y, z) = (x, y, f z)
