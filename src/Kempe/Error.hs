{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Error ( Error (..)
                   ) where

import           Control.DeepSeq   (NFData)
import           Control.Exception (Exception)
import           Data.Typeable     (Typeable)
import           GHC.Generics      (Generic)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter     (Pretty (pretty), comma, squotes, (<+>))

-- reject mutually recursive types? idk :p
data Error a = PoorScope a (Name a)
             | MismatchedLengths a (StackType a) (StackType a)
             | UnificationFailed a (KempeTy a) (KempeTy a) -- TODO: include atom expression?
             | TyVarExt a (Name a)
             | MonoFailed a
             | LessGeneral a (StackType a) (StackType a)
             | InvalidCExport a (Name a)
             | InvalidCImport a (Name a)
             | IllKinded a (KempeTy a)
             | BadType a
             deriving (Generic, NFData)

instance (Pretty a) => Show (Error a) where
    show = show . pretty

instance Pretty (Error a) where
    pretty (PoorScope _ n)               = "name" <+> squotes (pretty n) <+> "not in scope"
    pretty (MismatchedLengths _ st0 st1) = "mismatched type lengths" <+> pretty st0 <> comma <+> pretty st1
    pretty (UnificationFailed _ ty ty')  = "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (TyVarExt _ n)                = "Error in function" <+> pretty n <> ": type variables may not occur in external or exported functions."
    pretty (MonoFailed _)                = "Monomorphization step failed"
    pretty (LessGeneral _ sty sty')      = "Type" <+> pretty sty' <+> "is not as general as type" <+> pretty sty
    pretty (InvalidCExport _ n)          = "C export" <+> pretty n <+> "has more than one return value"
    pretty (InvalidCImport _ n)          = pretty n <+> "imported functions can have at most one return value"
    pretty (IllKinded _ ty)              = "Ill-kinded type:" <+> squotes (pretty ty) <+> ". Note that type variables have kind ⭑ in Kempe."
    pretty (BadType _)                   = "All types appearing in a signature must have kind ⭑"

instance (Pretty a, Typeable a) => Exception (Error a)
