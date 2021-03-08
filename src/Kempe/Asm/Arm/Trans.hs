module Kempe.Asm.Arm.Trans ( irToAarch64
                           ) where

import           Data.Foldable.Ext  (foldMapA)
import           Kempe.AST.Size
import           Kempe.Asm.Arm.Type
import           Kempe.IR.Monad
import qualified Kempe.IR.Type      as IR

irToAarch64 :: SizeEnv -> IR.WriteSt -> [IR.Stmt] -> [Arm AbsReg ()]
irToAarch64 env w = runWriteM w . foldMapA (irEmit env)

allocReg :: WriteM AbsReg
allocReg = AllocReg <$> getInt

-- example function call (arm) https://www.cs.princeton.edu/courses/archive/spr19/cos217/lectures/15_AssemblyFunctions.pdf

irEmit :: SizeEnv -> IR.Stmt -> WriteM [Arm AbsReg ()]
irEmit _ (IR.Jump l) = pure [Branch () l]
irEmit _ IR.Ret      = pure [Ret ()]
