module Kempe.KeystonePipeline ( assembleAtomX86
                              ) where

import qualified Data.ByteString           as BS
import           Kempe.AST
import           Kempe.AST.Size
import           Kempe.Asm.Liveness        (reconstruct)
import qualified Kempe.Asm.X86.ControlFlow as X86
import           Kempe.Asm.X86.Keystone    (assembleX86)
import qualified Kempe.Asm.X86.Linear      as X86
import           Kempe.Asm.X86.Trans
import           Kempe.IR

-- throws error
assembleAtomX86 :: SizeEnv
                -> Atom (ConsAnn MonoStackType) MonoStackType
                -> BS.ByteString
assembleAtomX86 sz a =
    let (stmts, st) = runTempM (writeAtom sz True a)
        in handleErr $ assembleX86 $ X86.allocRegs $ reconstruct $ X86.mkControlFlow $ irToX86 sz st stmts
    where handleErr = either (error . show) id
