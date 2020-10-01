module Kempe.TypeSynthesis ( catTypes
                           ) where

import           Kempe.AST

-- should types of e.g. atoms be StackType a -> StackType a ?

catTypes :: StackType a -> StackType a -> StackType a
catTypes _ _ = undefined
