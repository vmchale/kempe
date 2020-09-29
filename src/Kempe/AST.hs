module Kempe.AST ( BuiltinTy (..)
                 , KempeTy (..)
                 , TyDecl (..)
                 ) where

import           Kempe.Name

data BuiltinTy = TyPtr
               | TyInt
               -- -- | TyArr !Word

-- what to do about if, dip
--
-- special cases w.r.t. codegen
-- dk what tensor types are (or morphisms) but they look cool?
--
-- recursion > while loop (polymorphic recursion though :o )
--
-- equality for sum types &c.
--
-- what about pattern matches that bind variables??

data KempeTy a = TyBuiltin a !BuiltinTy
               | TyNamed a (TyName a)
               | TyTuple a [KempeTy a]
               | TyApp a (KempeTy a) (KempeTy a)
               -- | TyUniversal a (Name a) (KempeTy a)

data TyDecl a = TyDecl a (TyName a) [Name a] [(Name a, [TyName a])]
