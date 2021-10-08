module Kempe.Asm.Arm.PreAlloc ( stripRedundantDefines
                              ) where

import qualified Data.IntSet        as IS
import           Kempe.Asm.Arm.Type
import           Kempe.Asm.Type

-- | Filter out loads (@ldr@, @ldrb@) that write to a register which is not
-- subsequently used.
stripRedundantDefines :: [Arm reg Liveness] -> [Arm reg Liveness]
stripRedundantDefines = filter doesn'tLoadNothing

-- FIXME: we want this more general: any instruction which "defines" a value
-- which isn't actually live-out or w/e...
-- (otherwise it will cause problems with register allocation down the line)
doesn'tLoadNothing :: Arm reg Liveness -> Bool
doesn'tLoadNothing (Load (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (LoadByte (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (LoadLabel (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing _ = True
