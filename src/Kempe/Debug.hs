module Kempe.Debug ( armDebug
                   ) where

import qualified Kempe.Asm.Arm.ControlFlow as Arm
import qualified Kempe.Asm.Arm.PreAlloc    as Arm
import qualified Kempe.Asm.Arm.Type        as Arm
import           Kempe.Asm.Liveness
import           Kempe.Module
import           Kempe.Pipeline
import           Prettyprinter             (Doc)

-- | Helper function displays calculated live ranges for debugging
armDebug :: FilePath -> IO (Doc ann)
armDebug fp =
      Arm.prettyDebugAsm
    . Arm.stripRedundantDefines
    . reconstruct
    . Arm.mkControlFlow
    . uncurry armParsed <$> parseProcess fp
