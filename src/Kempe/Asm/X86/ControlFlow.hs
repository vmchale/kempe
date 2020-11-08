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

next :: [X86 () reg] -> FreshM ([Int] -> [Int], [X86 ControlAnn reg])
next asms = do
    nextAsms <- mkControlFlow asms
    case nextAsms of
        []            -> pure (id, [])
        asms'@(asm:_) -> pure ((node (ann asm) :), asms')

-- | Annotate instructions with a unique node name and a map to all possible
-- destinations
--
-- FIXME: two-pass, one to map labels
-- lol tardis
mkControlFlow :: [X86 () reg] -> FreshM [X86 ControlAnn reg]
mkControlFlow [] = pure []
mkControlFlow ((Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f [])) l : asms')
    }
mkControlFlow ((Ret _):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure (Ret (ControlAnn i (f [])) : asms')
    }
mkControlFlow ((PushReg _ r):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure (PushReg (ControlAnn i (f [])) r : asms')
    }
