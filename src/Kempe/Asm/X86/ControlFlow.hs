module Kempe.Asm.X86.ControlFlow ( mkControlFlow
                                 ) where

import           Control.Monad.State (State, get, modify)
import           Kempe.Asm.X86

type FreshM = State Int -- TODO: lookup?

data ControlAnn = ControlAnn { node :: !Int
                             , conn :: [Int]
                             }

getFresh :: FreshM Int
getFresh = get <* modify (+1)

-- TODO: backwards so cataM?

mkControlFlow :: [X86 () reg] -> FreshM [X86 ControlAnn reg]
mkControlFlow = undefined
