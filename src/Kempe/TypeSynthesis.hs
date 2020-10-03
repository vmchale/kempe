module Kempe.TypeSynthesis ( catTypes
                           ) where

import qualified Data.IntMap as IM
import           Kempe.AST

-- should types of e.g. atoms be StackType a -> StackType a ?
--
-- alpha-equivalence (of 'StackType's?) (note it is quantified *only* on the "exterior" i.e.
-- implicitly)

-- how does one scope unification "back"? (tardis monad but I don't want it to
-- hang indefinitely...)
--
-- also monomorphization
type TyEnv a = IM.IntMap (KempeTy a)

-- TODO: need a renamer for types
typeOfBuiltin :: BuiltinFn -> StackType ()
typeOfBuiltin Drop = undefined
typeOfBuiltin Swap = undefined

-- | Given @x@ and @y@, return the 'StackType' of @xy@
catTypes :: StackType a -- ^ @x@
         -> StackType a -- ^ @y@
         -> StackType a
catTypes _ _ = undefined -- I need unification? :o
