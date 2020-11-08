module Kempe.Asm.X86.ControlFlow ( mkControlFlow
                                 ) where

import           Control.Monad.State (State, gets, modify)
import           Data.Bifunctor      (first, second)
import qualified Data.Map            as M
import           Kempe.Asm.X86.Type

-- map of labels by node (maybe backwards?)
type FreshM = State (Int, M.Map Label Int)

data ControlAnn = ControlAnn { node :: !Int
                             , conn :: [Int]
                             }

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

-- TODO: backwards so cataM?

mkControlFlow :: [X86 () reg] -> FreshM [X86 ControlAnn reg]
mkControlFlow ((Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; asmsAnn <- mkControlFlow asms
    ; case asmsAnn of
        []            -> pure [Label (ControlAnn i []) l]
        asms'@(asm:_) -> pure $ Label (ControlAnn i [node $ ann asm]) l : asms'
    }
