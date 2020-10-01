module Kempe.TypeSynthesis ( catTypes
                           ) where

import           Kempe.AST

catTypes :: StackType a -> StackType a -> StackType a
catTypes _ _ = undefined
