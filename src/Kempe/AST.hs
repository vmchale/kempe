module Kempe.AST ( TyDecl (..)
                 ) where

import           Kempe.Name

data TyDecl a = TyDecl a (TyName a) [Name a]
