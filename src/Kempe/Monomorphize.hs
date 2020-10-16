{-# LANGUAGE OverloadedStrings #-}

module Kempe.Monomorphize ( monomorphize
                          ) where

import           Data.Functor   (void)
import qualified Data.IntSet    as IS
import           Data.Semigroup ((<>))
import qualified Data.Set       as S
import           Data.Text      as T
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name

type MonoStackType = ([KempeTy ()], [KempeTy ()])

squishTypeName :: BuiltinTy -> T.Text
squishTypeName TyPtr  = "ptr"
squishTypeName TyInt  = "int"
squishTypeName TyBool = "bool"

squishType :: KempeTy a -> T.Text
squishType (TyBuiltin _ b)          = squishTypeName b
squishType (TyNamed _ (Name t _ _)) = T.toLower t
squishType TyVar{}                  = error "not meant to be monomorphized!"
squishType (TyTuple _ tys)          = foldMap squishType tys
squishType (TyApp _ ty ty')         = squishType ty <> squishType ty'

-- all names + their type at call site
type Used = S.Set (Name (), StackType ()) -- should this contain only polymorphic stuff?

usedAtom :: Atom (StackType ()) -> Used
usedAtom AtBuiltin{}                = mempty
usedAtom BoolLit{}                  = mempty
usedAtom IntLit{}                   = mempty
usedAtom (AtName _ n@(Name _ _ l))  = S.singleton (void n, l)
usedAtom (If _ as as')              = foldMap usedAtom as <> foldMap usedAtom as'
usedAtom (Dip _ as)                 = foldMap usedAtom as
usedAtom (AtCons _ tn@(Name _ _ l)) = S.singleton (void tn, l)

usedDecl :: KempeDecl () (StackType ()) -> Used
usedDecl TyDecl{}             = mempty
usedDecl ExtFnDecl{}          = mempty
usedDecl (FunDecl _ _ _ _ as) = foldMap usedAtom as

used :: Module () (StackType ()) -> Used
used = foldMap usedDecl

discard :: StackType () -> Either (Error ()) MonoStackType
discard (StackType qs is os) | S.null qs = Right (is, os)
                             | otherwise = Left $ TyVarExt ()

monomorphizeDecl :: KempeDecl () (StackType ()) -> Either (Error ()) (KempeDecl () MonoStackType)
monomorphizeDecl (ExtFnDecl l (Name t u l') is os cn) = ExtFnDecl <$> discard l <*> (Name t u <$> discard l') <*> undefined <*> undefined <*> pure cn

-- decide which versions we need?
monomorphize :: Module () (StackType ()) -> Either (Error ()) (Module () MonoStackType)
monomorphize = traverse monomorphizeDecl
