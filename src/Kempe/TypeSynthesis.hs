module Kempe.TypeSynthesis ( catTypes
                           ) where

import           Kempe.AST

-- should types of e.g. atoms be StackType a -> StackType a ?
--
-- alpha-equivalence (of 'StackType's?) (note it is quantified *only* on the "exterior" i.e.
-- implicitly)

-- | Given @x@ and @y@, return the 'StackType' of @xy@
catTypes :: StackType a -- ^ @x@
         -> StackType a -- ^ @y@
         -> StackType a
catTypes _ _ = undefined -- not actually easy because I need unification? :o
