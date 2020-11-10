module Kempe.Asm.X86.Liveness ( initLiveness
                              , Liveness
                              ) where

import           Data.Functor       (($>))
import qualified Data.Set           as S
import           Kempe.Asm.X86.Type

type Liveness = S.Set AbsReg

-- need: succ for a node

initLiveness :: [X86 AbsReg a] -> [X86 AbsReg Liveness]
initLiveness = fmap ($> S.empty)
