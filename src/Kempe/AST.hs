{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend AST
module Kempe.AST ( BuiltinTy (..)
                 , KempeTy (..)
                 , StackType (..)
                 , ConsAnn (..)
                 , Atom (..)
                 , BuiltinFn (..)
                 , KempeDecl (..)
                 , Pattern (..)
                 , ABI (..)
                 , Module
                 , freeVars
                 , MonoStackType
                 , size
                 , prettyMonoStackType
                 , prettyTyped
                 , prettyTypedModule
                 , prettyModule
                 -- * I resent this...
                 , voidStackType
                 ) where

import           Control.DeepSeq         (NFData)
import           Data.Bifoldable         (Bifoldable (bifoldMap))
import           Data.Bifunctor          (Bifunctor (..))
import           Data.Bitraversable      (Bitraversable (..))
import qualified Data.ByteString.Lazy    as BSL
import           Data.Functor            (void)
import           Data.Int                (Int64, Int8)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Set                as S
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Word               (Word8)
import           GHC.Generics            (Generic)
import           Kempe.Name
import           Numeric.Natural
import           Prettyprinter           (Doc, Pretty (pretty), align, braces, brackets, concatWith, fillSep, hsep, parens, pipe, sep, tupled, (<+>))

data BuiltinTy = TyPtr
               | TyInt
               | TyBool
               | TyInt8
               | TyWord
               -- -- | TyFloat
               -- -- | TyArr Word
               deriving (Generic, NFData, Eq, Ord)
               -- tupling builtin for sake of case-matching on two+ things at
               -- once
               --
               -- #1 vs ->1 (lol)

instance Pretty BuiltinTy where
    pretty TyPtr  = "Ptr"
    pretty TyInt  = "Int"
    pretty TyBool = "Bool"
    pretty TyInt8 = "Int8"
    pretty TyWord = "Word"

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
               | TyApp a (KempeTy a) (KempeTy a) -- type applied to another, e.g. Just Int
               | TyTuple a [KempeTy a]
               deriving (Generic, NFData, Functor, Eq, Ord) -- questionable eq instance but eh

data StackType b = StackType { quantify :: S.Set (Name b)
                             , inTypes  :: [KempeTy b]
                             , outTypes :: [KempeTy b]
                             } deriving (Generic, NFData, Eq, Ord)

type MonoStackType = ([KempeTy ()], [KempeTy ()])

-- | Annotation carried on constructors to keep size information through the IR
-- generation phase.
data ConsAnn a = ConsAnn { tySz :: Int64, tag :: Word8, consTy :: a }
    deriving (Functor, Foldable, Traversable, Generic, NFData)

prettyMonoStackType :: MonoStackType -> Doc a
prettyMonoStackType (is, os) = sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os)

instance Pretty (StackType a) where
    pretty (StackType _ ins outs) = sep (fmap pretty ins) <+> "--" <+> sep (fmap pretty outs)

voidStackType :: StackType a -> StackType ()
voidStackType (StackType vars ins outs) = StackType (S.map void vars) (void <$> ins) (void <$> outs)

instance Pretty (KempeTy a) where
    pretty (TyBuiltin _ b)  = pretty b
    pretty (TyNamed _ tn)   = pretty tn
    pretty (TyVar _ n)      = pretty n
    pretty (TyApp _ ty ty') = parens (pretty ty <+> pretty ty')
    pretty (TyTuple _ tys)  = tupled (pretty <$> tys)

data Pattern b = PatternInt b Integer
               | PatternCons b (TyName b) -- a constructed pattern
               | PatternWildcard b
               | PatternBool b Bool
               -- -- | PatternTuple
               deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

instance Pretty (Pattern a) where
    pretty (PatternInt _ i)   = pretty i
    pretty (PatternBool _ b)  = pretty b
    pretty PatternWildcard{}  = "_"
    pretty (PatternCons _ tn) = pretty tn

instance Pretty (Atom a) where
    pretty (AtName _ n)    = pretty n
    pretty (Dip _ as)      = "dip(" <> fillSep (fmap pretty as) <> ")"
    pretty (AtBuiltin _ b) = pretty b
    pretty (AtCons _ tn)   = pretty tn
    pretty (If _ as as')   = "if(" <> align (fillSep (fmap pretty as)) <> ", " <> align (fillSep (fmap pretty as')) <> ")"
    pretty (IntLit _ i)    = pretty i
    pretty (BoolLit _ b)   = pretty b
    pretty (WordLit _ w)   = pretty w <> "u"
    pretty (Int8Lit _ i)   = pretty i <> "i8"

prettyTyped :: Atom (StackType ()) -> Doc ann
prettyTyped (AtName ty n)    = parens (pretty n <+> ":" <+> pretty ty)
prettyTyped (Dip _ as)       = "dip(" <> fillSep (prettyTyped <$> as) <> ")"
prettyTyped (AtBuiltin ty b) = parens (pretty b <+> ":" <+> pretty ty)
prettyTyped (AtCons ty tn)   = parens (pretty tn <+> ":" <+> pretty ty)
prettyTyped (If _ as as')    = "if(" <> align (fillSep (prettyTyped <$> as)) <> ", " <> align (fillSep (prettyTyped <$> as')) <> ")"
prettyTyped (IntLit _ i)     = pretty i
prettyTyped (BoolLit _ b)    = pretty b
prettyTyped (Int8Lit _ i)    = pretty i <> "i8"
prettyTyped (WordLit _ n)    = pretty n <> "u"

data Atom b = AtName b (Name b)
            | Case b (NonEmpty (Pattern b, [Atom b]))
            | If b [Atom b] [Atom b]
            | Dip b [Atom b]
            | IntLit b Integer
            | WordLit b Natural
            | Int8Lit b Int8
            | BoolLit b Bool
            | AtBuiltin b BuiltinFn
            | AtCons b (TyName b)
            deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

data BuiltinFn = Drop
               | Swap
               | Dup
               | IntPlus
               | IntMinus
               | IntTimes
               | IntDiv
               | IntMod
               | IntEq
               | IntShiftR
               | IntShiftL
               | IntXor
               | WordPlus
               | WordTimes
               | WordShiftR
               | WordShiftL
               | WordXor
               -- TODO: IntLt and such
               deriving (Eq, Generic, NFData)

instance Pretty BuiltinFn where
    pretty Drop       = "drop"
    pretty Swap       = "swap"
    pretty Dup        = "dup"
    pretty IntPlus    = "+"
    pretty IntMinus   = "-"
    pretty IntTimes   = "*"
    pretty IntDiv     = "/"
    pretty IntMod     = "%"
    pretty IntEq      = "="
    pretty IntShiftR  = ">>"
    pretty IntShiftL  = "<<"
    pretty WordPlus   = "+~"
    pretty WordTimes  = "*~"
    pretty WordShiftL = "<<~"
    pretty WordShiftR = ">>~"
    pretty IntXor     = "xori"
    pretty WordXor    = "xoru"

data ABI = Cabi
         | Kabi
         deriving (Eq, Generic, NFData)

instance Pretty ABI where
    pretty Cabi = "cabi"
    pretty Kabi = "kabi"

prettyKempeDecl :: (Atom b -> Doc ann) -> KempeDecl a c b -> Doc ann
prettyKempeDecl atomizer (FunDecl _ n is os as) = pretty n <+> ":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <+> "=:" <+> brackets (align (fillSep (atomizer <$> as)))
prettyKempeDecl _ (Export _ abi n)              = "%foreign" <+> pretty abi <+> pretty n
prettyKempeDecl _ (ExtFnDecl _ n is os b)       = pretty n <+> ":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <+> "=:" <+> "$cfun" <> pretty (decodeUtf8 b)
prettyKempeDecl _ (TyDecl _ tn ns ls)           = "type" <+> pretty tn <+> hsep (fmap pretty ns) <+> braces (concatWith (\x y -> x <+> pipe <+> y) $ fmap (uncurry prettyTyLeaf) ls)

instance Pretty (KempeDecl a b c) where
    pretty = prettyKempeDecl pretty

prettyTyLeaf :: TyName a -> [KempeTy b] -> Doc ann
prettyTyLeaf cn vars = pretty cn <+> hsep (fmap pretty vars)

-- TODO: separate annotations for TyName in TyDecl
data KempeDecl a c b = TyDecl a (TyName a) [Name a] [(TyName c, [KempeTy a])]
                     | FunDecl b (Name b) [KempeTy a] [KempeTy a] [Atom b]
                     | ExtFnDecl b (Name b) [KempeTy a] [KempeTy a] BSL.ByteString -- ShortByteString?
                     | Export b ABI (Name b)
                     deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor (KempeDecl a) where
    first f (TyDecl x tn ns ls)        = TyDecl x tn ns (fmap (first (fmap f)) ls)
    first _ (FunDecl l n tys tys' as)  = FunDecl l n tys tys' as
    first _ (ExtFnDecl l n tys tys' b) = ExtFnDecl l n tys tys' b
    first _ (Export l abi n)           = Export l abi n
    second = fmap

instance Bifoldable (KempeDecl a) where
    bifoldMap f _ (TyDecl _ _ _ ls)          = foldMap (foldMap f) (fst <$> ls)
    bifoldMap _ g (FunDecl x n tys tys' a)   = foldMap g (FunDecl x n tys tys' a)
    bifoldMap _ g (ExtFnDecl l n tys tys' b) = foldMap g (ExtFnDecl l n tys tys' b)
    bifoldMap _ g (Export l abi n)           = foldMap g (Export l abi n)

instance Bitraversable (KempeDecl a) where
    bitraverse f _ (TyDecl l tn ns ls)        =
        let as = snd <$> ls
            constrs = fst <$> ls
            in TyDecl l tn ns . flip zip as <$> traverse (traverse f) constrs
    bitraverse _ g (FunDecl x n tys tys' a)   = traverse g (FunDecl x n tys tys' a)
    bitraverse _ g (ExtFnDecl l n tys tys' b) = traverse g (ExtFnDecl l n tys tys' b)
    bitraverse _ g (Export l abi n)           = traverse g (Export l abi n)

prettyModuleGeneral :: (Atom b -> Doc ann) -> Module a c b -> Doc ann
prettyModuleGeneral atomizer = sep . fmap (prettyKempeDecl atomizer)

prettyTypedModule :: Module () (StackType ()) (StackType ()) -> Doc ann
prettyTypedModule = prettyModuleGeneral prettyTyped

prettyModule :: Module a c b -> Doc ann
prettyModule = prettyModuleGeneral pretty

type Module a c b = [KempeDecl a c b]

extrVars :: KempeTy a -> [Name a]
extrVars TyBuiltin{}      = []
extrVars TyNamed{}        = []
extrVars (TyVar _ n)      = [n]
extrVars (TyApp _ ty ty') = extrVars ty ++ extrVars ty'
extrVars (TyTuple _ tys)  = concatMap extrVars tys

freeVars :: [KempeTy a] -> S.Set (Name a)
freeVars tys = S.fromList (concatMap extrVars tys)

-- | Don't call this on ill-kinded types; it won't throw any error.
size :: KempeTy a -> Int64
size (TyBuiltin _ TyInt)      = 8 -- since we're only targeting x86_64 and aarch64 we have 64-bit 'Int's
size (TyBuiltin _ TyPtr)      = 8
size (TyBuiltin _ TyBool)     = 1
size (TyBuiltin _ TyInt8)     = 1
size (TyBuiltin _ TyWord)     = 8
size TyVar{}                  = error "Internal error: type variables should not be present at this stage."
size (TyTuple _ tys)          = sum (fmap size tys)
size TyNamed{}                = 1
size (TyApp _ TyNamed{} ty)   = 1 + size ty
size (TyApp _ ty@TyApp{} ty') = size ty + size ty'
size (TyApp _ TyBuiltin{} _)  = error "Internal error: ill-kinded type!"
size (TyApp _ TyVar{} _)      = error "Internal error: type variables should not be present at this stage."
size (TyApp _ TyTuple{} _)    = error "Internal error: ill-kinded type!"
