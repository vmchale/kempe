module Kempe.AST ( BuiltinTy (..)
                 , KempeTy (..)
                 , TyDecl (..)
                 ) where

import           Kempe.Name

data BuiltinTy = TyPtr
               | TyInt
               -- -- | TyArr !Word
               -- tupling builtin for sake of case-matching on two+ things at
               -- once

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

data KempeTy a = TyBuiltin a BuiltinTy
               | TyNamed a (TyName a)
               | TyVar a (Name a)
               | TyTuple a [KempeTy a]
               | TyApp a (KempeTy a) (KempeTy a)

data TyDecl a = TyDecl a (TyName a) [Name a] [(TyName a, [KempeTy a])]

data Pattern a

data Atom a = AtName a (Name a)
            | Ccall a (Name a)
            | Case a [(Pattern a, Atom a)]
            | IntLit a Integer

data FunDecl a = FunDecl a (Name a) (KempeTy a) (KempeTy a) [Atom a]
