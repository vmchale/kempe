{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Kempe.Asm.X86.ControlFlow ( mkControlFlow
                                 , ControlAnn (..)
                                 ) where

import           Control.DeepSeq     (NFData)
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.Bifunctor      (first, second)
import           Data.Functor        (($>))
import qualified Data.Map            as M
import           GHC.Generics        (Generic)
import           Kempe.Asm.X86.Type

-- map of labels by node (maybe backwards?)
type FreshM = State (Int, M.Map Label Int)

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

data ControlAnn = ControlAnn { node :: !Int
                             , conn :: [Int]
                             } deriving (Generic, NFData)

mkControlFlow :: [X86 reg ()] -> [X86 reg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

-- | Annotate instructions with a unique node name and a map to all possible
-- destinations
addControlFlow :: [X86 reg ()] -> FreshM [X86 reg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f [])) l : asms')
    }
addControlFlow ((Je _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l -- TODO: is this what's wanted?
    ; pure (Je (ControlAnn i (f [l_i])) l : asms')
    }
addControlFlow ((Jump _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Jump (ControlAnn i [l_i]) l : nextAsms)
    }
addControlFlow ((Call _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (Call (ControlAnn i [l_i]) l : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f [])) : asms')
    }

next :: [X86 reg ()] -> FreshM ([Int] -> [Int], [X86 reg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []            -> pure (id, [])
        asms'@(asm:_) -> pure ((node (ann asm) :), asms')

-- | Construct map assigning labels to their node name.
broadcasts :: [X86 reg ()] -> FreshM [X86 reg ()]
broadcasts [] = pure []
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
