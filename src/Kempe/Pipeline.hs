module Kempe.Pipeline ( irGen
                      ) where

import           Control.Exception (throw)
import           Kempe.AST
import           Kempe.IR
import           Kempe.Shuttle

irGen :: Int -- ^ Thread uniques through
      -> Module a b -> [Stmt ()]
irGen i m = runTempM (writeModule tAnnMod)
    where tAnnMod = either throw id $ monomorphize i m
