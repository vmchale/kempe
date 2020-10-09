{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend AST
module Kempe.AST ( BuiltinTy (..)
                 , KempeTy (..)
                 , StackType (..)
                 , Atom (..)
                 , BuiltinFn (..)
                 , KempeDecl (..)
                 , Pattern (..)
                 , Module
                 ) where

import           Control.DeepSeq      (NFData)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set             as S
import           GHC.Generics         (Generic)
import           Kempe.Name
import           Prettyprinter        (Pretty (pretty), parens, tupled, (<+>))

data BuiltinTy = TyPtr
               | TyInt
               | TyBool
               | TyArr Word
               deriving (Generic, NFData, Eq)
               -- tupling builtin for sake of case-matching on two+ things at
               -- once
               --
               -- #1 vs ->1 (lol)

instance Pretty BuiltinTy where
    pretty TyPtr  = "Ptr"
    pretty TyInt  = "Int"
    pretty TyBool = "Bool"

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
               | TyApp a (KempeTy a) (KempeTy a)
               | TyTuple a [KempeTy a]
               deriving (Generic, NFData)

data StackType a = StackType { quantify :: S.Set (Name a)
                             , inTypes  :: [KempeTy a]
                             , outTypes :: [KempeTy a]
                             }

instance Pretty (KempeTy a) where
    pretty (TyBuiltin _ b)  = pretty b
    pretty (TyNamed _ tn)   = pretty tn
    pretty (TyVar _ n)      = pretty n
    pretty (TyApp _ ty ty') = parens (pretty ty <+> pretty ty')
    pretty (TyTuple _ tys)  = tupled (pretty <$> tys)

data Pattern a = PatternInt a Integer
               | PatternCons a (TyName a) [Pattern a] -- a constructed pattern
               | PatternVar a (Name a)
               | PatternWildcard a
               deriving (Generic, NFData)

data Atom a = AtName a (Name a)
            | Ccall a BSL.ByteString -- TODO: type annotation?
            | Case a [(Pattern a, [Atom a])] -- TODO: should this be nonempty? hm
            | If a [Atom a] [Atom a]
            | Dip a [Atom a]
            | IntLit a Integer
            | BoolLit a Bool
            | AtBuiltin a BuiltinFn
            | AtCons a (TyName a)
            deriving (Generic, NFData)

data BuiltinFn = Drop
               | Swap
               deriving (Generic, NFData)

data KempeDecl a = TyDecl a (TyName a) [Name a] [(TyName a, [KempeTy a])]
                 | FunDecl a (Name a) [KempeTy a] [KempeTy a] [Atom a]
                 deriving (Generic, NFData)

type Module a = [KempeDecl a]
