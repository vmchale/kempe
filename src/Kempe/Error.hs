{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error ( Error (..)
                   ) where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception)
import           Data.Semigroup    ((<>))
import           Data.Typeable     (Typeable)
import           GHC.Generics      (Generic)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter     (Pretty (pretty), comma, squotes, (<+>))

-- reject mutually recursive types? idk :p
data Error a = PoorScope a (Name a)
             | MismatchedLengths a (StackType a) (StackType a)
             | UnificationFailed a (KempeTy a) (KempeTy a) -- TODO: include atom expression?
             | TyVarExt a
             | MonoFailed a
             deriving (Generic, NFData)

instance (Pretty a) => Show (Error a) where
    show = show . pretty

instance Pretty (Error a) where
    pretty (PoorScope _ n)               = "name" <+> squotes (pretty n) <+> "not in scope"
    pretty (MismatchedLengths _ st0 st1) = "mismatched type lengths" <+> pretty st0 <> comma <+> pretty st1
    pretty (UnificationFailed _ ty ty')  = "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (TyVarExt _)                  = "Type variables may not occur in external or exported functions."
    pretty (MonoFailed _)                = "Monomorphization step failed"

instance (Pretty a, Typeable a) => Exception (Error a)
