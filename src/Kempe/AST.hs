module Kempe.AST ( BuiltinTy (..)
                 , KempeTy (..)
                 ) where

import           Kempe.Name

data BuiltinTy = TyPtr
               | TyInt
               | TyArr !Word

data KempeTy a = TyBuiltin a !BuiltinTy
               | TyNamed a (TyName a)
               | TyUniversal a (Name a) (KempeTy a)
