-- | Put this in its own module to
module Kempe.IR.Monad ( WriteM
                      , nextLabels
                      , nextInt
                      , getInt
                      , getLabel
                      , runWriteM
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

runWriteM :: WriteSt -> WriteM a -> a
runWriteM = flip evalState
