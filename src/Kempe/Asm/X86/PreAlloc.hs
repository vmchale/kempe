module Kempe.Asm.X86.PreAlloc ( stripRedundantDefines
                              ) where

import qualified Data.IntSet        as IS
import           Kempe.Asm.Type
import           Kempe.Asm.X86.Type

-- | Filter out @mov@s that write to a register which is not
-- subsequently used.
stripRedundantDefines :: [X86 reg Liveness] -> [X86 reg Liveness]
stripRedundantDefines = filter doesn'tLoadNothing

-- FIXME: we want this more general: any instruction which "defines" a value
-- which isn't actually live-out or w/e...
-- (otherwise it will cause problems with register allocation down the line)
doesn'tLoadNothing :: X86 reg Liveness -> Bool
doesn'tLoadNothing (MovRA (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRR (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRRLower (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRL (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRC (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRCTag (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRCBool (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing (MovRWord (Liveness _ defs) _ _) | IS.null defs = False
doesn'tLoadNothing _ = True
