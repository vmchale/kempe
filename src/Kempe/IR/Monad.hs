-- | Put this in its own module to
module Kempe.IR.Monad ( WriteM
                      , nextLabels
                      , nextInt
                      , getInt
                      , getLabel
                      , runWriteM
                      , allocTemp8
                      , allocTemp64
                      ) where

import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Kempe.IR.Type

type WriteM = State WriteSt

nextLabels :: WriteSt -> WriteSt
nextLabels (WriteSt ls ts) = WriteSt (tail ls) ts

nextInt :: WriteSt -> WriteSt
nextInt (WriteSt ls ts) = WriteSt ls (tail ts)

getInt :: WriteM Int
getInt = gets (head . temps) <* modify nextInt

getLabel :: WriteM Label
getLabel = gets (head . wlabels) <* modify nextLabels

allocTemp64 :: WriteM Temp
allocTemp64 = Temp64 <$> getInt

allocTemp8 :: WriteM Temp
allocTemp8 = Temp8 <$> getInt

runWriteM :: WriteSt -> WriteM a -> a
runWriteM = flip evalState
