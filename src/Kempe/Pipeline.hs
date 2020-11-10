module Kempe.Pipeline ( irGen
                      , x86Parsed
                      ) where

import           Control.Exception (throw)
import           Kempe.AST
import           Kempe.Asm.X86
import           Kempe.IR
import           Kempe.Shuttle

irGen :: Int -- ^ Thread uniques through
      -> Module a b -> ([Stmt ()], WriteSt)
irGen i m = runTempM (writeModule tAnnMod)
    where tAnnMod = either throw id $ monomorphize i m


x86Parsed :: Int -> Module a b -> [X86 AbsReg ()]
x86Parsed i m = let (ir, u) = irGen i m in irToX86 u ir
