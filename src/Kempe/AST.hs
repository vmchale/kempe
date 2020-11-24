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
                 , sizeStack
                 , prettyMonoStackType
                 , prettyTyped
                 , prettyTypedModule
                 , prettyFancyModule
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
import qualified Data.List.NonEmpty      as NE
import           Data.Monoid             (Sum (..))
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

data Pattern c b = PatternInt b Integer
                 | PatternCons c (TyName c) -- a constructed pattern
                 | PatternWildcard b
                 | PatternBool b Bool
                 -- -- | PatternTuple
                 deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor Pattern where
    second = fmap
    first f (PatternCons l tn)  = PatternCons (f l) (fmap f tn)
    first _ (PatternInt l i)    = PatternInt l i
    first _ (PatternWildcard l) = PatternWildcard l
    first _ (PatternBool l b)   = PatternBool l b

instance Bifoldable Pattern where
    bifoldMap _ g (PatternInt l i)    = foldMap g (PatternInt l i)
    bifoldMap _ g (PatternWildcard l) = foldMap g (PatternWildcard l)
    bifoldMap _ g (PatternBool l b)   = foldMap g (PatternBool l b)
    bifoldMap f _ (PatternCons l tn)  = f l <> foldMap f tn

instance Bitraversable Pattern where
    bitraverse _ g (PatternInt l i)    = traverse g (PatternInt l i)
    bitraverse _ g (PatternWildcard l) = traverse g (PatternWildcard l)
    bitraverse _ g (PatternBool l b)   = traverse g (PatternBool l b)
    bitraverse f _ (PatternCons l cn)  = PatternCons <$> f l <*> traverse f cn

instance Pretty (Pattern c a) where
    pretty (PatternInt _ i)   = pretty i
    pretty (PatternBool _ b)  = pretty b
    pretty PatternWildcard{}  = "_"
    pretty (PatternCons _ tn) = pretty tn

instance Pretty (Atom c a) where
    pretty (AtName _ n)    = pretty n
    pretty (Dip _ as)      = "dip(" <> fillSep (fmap pretty as) <> ")"
    pretty (AtBuiltin _ b) = pretty b
    pretty (AtCons _ tn)   = pretty tn
    pretty (If _ as as')   = "if(" <> align (fillSep (fmap pretty as)) <> ", " <> align (fillSep (fmap pretty as')) <> ")"
    pretty (IntLit _ i)    = pretty i
    pretty (BoolLit _ b)   = pretty b
    pretty (WordLit _ w)   = pretty w <> "u"
    pretty (Int8Lit _ i)   = pretty i <> "i8"

prettyTyped :: Atom (StackType ()) (StackType ()) -> Doc ann
prettyTyped (AtName ty n)    = parens (pretty n <+> ":" <+> pretty ty)
prettyTyped (Dip _ as)       = "dip(" <> fillSep (prettyTyped <$> as) <> ")"
prettyTyped (AtBuiltin ty b) = parens (pretty b <+> ":" <+> pretty ty)
prettyTyped (AtCons ty tn)   = parens (pretty tn <+> ":" <+> pretty ty)
prettyTyped (If _ as as')    = "if(" <> align (fillSep (prettyTyped <$> as)) <> ", " <> align (fillSep (prettyTyped <$> as')) <> ")"
prettyTyped (IntLit _ i)     = pretty i
prettyTyped (BoolLit _ b)    = pretty b
prettyTyped (Int8Lit _ i)    = pretty i <> "i8"
prettyTyped (WordLit _ n)    = pretty n <> "u"

data Atom c b = AtName b (Name b)
              | Case b (NonEmpty (Pattern c b, [Atom c b]))
              | If b [Atom c b] [Atom c b]
              | Dip b [Atom c b]
              | IntLit b Integer
              | WordLit b Natural
              | Int8Lit b Int8
              | BoolLit b Bool
              | AtBuiltin b BuiltinFn
              | AtCons c (TyName c)
              deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor Atom where
    second = fmap
    first f (AtCons l n)    = AtCons (f l) (fmap f n)
    first _ (AtName l n)    = AtName l n
    first _ (IntLit l i)    = IntLit l i
    first _ (WordLit l w)   = WordLit l w
    first _ (Int8Lit l i)   = Int8Lit l i
    first _ (BoolLit l b)   = BoolLit l b
    first _ (AtBuiltin l b) = AtBuiltin l b
    first f (Dip l as)      = Dip l (fmap (first f) as)
    first f (If l as as')   = If l (fmap (first f) as) (fmap (first f) as')
    first f (Case l ls)     =
        let (ps, aLs) = NE.unzip ls
            in Case l $ NE.zip (fmap (first f) ps) (fmap (fmap (first f)) aLs)

instance Bifoldable Atom where
    bifoldMap _ g (AtName x n)    = foldMap g (AtName x n)
    bifoldMap _ g (IntLit l i)    = foldMap g (IntLit l i)
    bifoldMap _ g (Int8Lit l i)   = foldMap g (Int8Lit l i)
    bifoldMap _ g (WordLit l w)   = foldMap g (WordLit l w)
    bifoldMap _ g (BoolLit l b)   = foldMap g (BoolLit l b)
    bifoldMap _ g (AtBuiltin l b) = foldMap g (AtBuiltin l b)
    bifoldMap f _ (AtCons l c)    = f l <> foldMap f c
    bifoldMap f g (Dip l as)      = g l <> foldMap (bifoldMap f g) as
    bifoldMap f g (If l as as')   = g l <> foldMap (bifoldMap f g) as <> foldMap (bifoldMap f g) as'
    bifoldMap f g (Case l ls)     =
        let (ps, as) = NE.unzip ls
            in g l <> foldMap (bifoldMap f g) ps <> foldMap (foldMap (bifoldMap f g)) as

instance Bitraversable Atom where
    bitraverse _ g (AtName x n)    = traverse g (AtName x n)
    bitraverse _ g (IntLit l i)    = traverse g (IntLit l i)
    bitraverse _ g (Int8Lit l i)   = traverse g (Int8Lit l i)
    bitraverse _ g (WordLit l w)   = traverse g (WordLit l w)
    bitraverse _ g (BoolLit l b)   = traverse g (BoolLit l b)
    bitraverse _ g (AtBuiltin l b) = traverse g (AtBuiltin l b)
    bitraverse f _ (AtCons l c)    = AtCons <$> f l <*> traverse f c
    bitraverse f g (Dip l as)      = Dip <$> g l <*> traverse (bitraverse f g) as
    bitraverse f g (If l as as')   = If <$> g l <*> traverse (bitraverse f g) as <*> traverse (bitraverse f g) as'
    bitraverse f g (Case l ls)     =
        let (ps, as) = NE.unzip ls
            in Case <$> g l <*> (NE.zip <$> traverse (bitraverse f g) ps <*> traverse (traverse (bitraverse f g)) as)

data BuiltinFn = Drop
               | Swap
               | Dup
               | IntPlus
               | IntMinus
               | IntTimes
               | IntDiv
               | IntMod
               | IntEq
               | IntLeq
               | IntLt
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
    pretty IntLeq     = "≤"
    pretty IntLt      = "<"
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

prettyKempeDecl :: (Atom c b -> Doc ann) -> KempeDecl a c b -> Doc ann
prettyKempeDecl atomizer (FunDecl _ n is os as) = pretty n <+> ":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <+> "=:" <+> brackets (align (fillSep (atomizer <$> as)))
prettyKempeDecl _ (Export _ abi n)              = "%foreign" <+> pretty abi <+> pretty n
prettyKempeDecl _ (ExtFnDecl _ n is os b)       = pretty n <+> ":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <+> "=:" <+> "$cfun" <> pretty (decodeUtf8 b)
prettyKempeDecl _ (TyDecl _ tn ns ls)           = "type" <+> pretty tn <+> hsep (fmap pretty ns) <+> braces (concatWith (\x y -> x <+> pipe <+> y) $ fmap (uncurry prettyTyLeaf) ls)

instance Pretty (KempeDecl a b c) where
    pretty = prettyKempeDecl pretty

prettyTyLeaf :: TyName a -> [KempeTy b] -> Doc ann
prettyTyLeaf cn vars = pretty cn <+> hsep (fmap pretty vars)

-- TODO: separate annotations for TyName in TyDecl
data KempeDecl a c b = TyDecl a (TyName a) [Name a] [(TyName b, [KempeTy a])]
                     | FunDecl b (Name b) [KempeTy a] [KempeTy a] [Atom c b]
                     | ExtFnDecl b (Name b) [KempeTy a] [KempeTy a] BSL.ByteString -- ShortByteString?
                     | Export b ABI (Name b)
                     deriving (Eq, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor (KempeDecl a) where
    first _ (TyDecl x tn ns ls)        = TyDecl x tn ns ls
    first f (FunDecl l n tys tys' as)  = FunDecl l n tys tys' (fmap (first f) as)
    first _ (ExtFnDecl l n tys tys' b) = ExtFnDecl l n tys tys' b
    first _ (Export l abi n)           = Export l abi n
    second = fmap

instance Bifoldable (KempeDecl a) where
    bifoldMap _ g (TyDecl x tn ns ls)        = foldMap g (TyDecl x tn ns ls)
    bifoldMap _ g (ExtFnDecl l n tys tys' b) = foldMap g (ExtFnDecl l n tys tys' b)
    bifoldMap _ g (Export l abi n)           = foldMap g (Export l abi n)
    bifoldMap f g (FunDecl x n _ _ a)        = g x <> foldMap g n <> foldMap (bifoldMap f g) a

instance Bitraversable (KempeDecl a) where
    bitraverse _ g (TyDecl l tn ns ls)        = traverse g (TyDecl l tn ns ls)
    bitraverse f g (FunDecl x n tys tys' a)   = FunDecl <$> g x <*> traverse g n <*> pure tys <*> pure tys' <*> traverse (bitraverse f g) a
    bitraverse _ g (ExtFnDecl l n tys tys' b) = traverse g (ExtFnDecl l n tys tys' b)
    bitraverse _ g (Export l abi n)           = traverse g (Export l abi n)

prettyModuleGeneral :: (Atom c b -> Doc ann) -> Module a c b -> Doc ann
prettyModuleGeneral atomizer = sep . fmap (prettyKempeDecl atomizer)

prettyFancyModule :: Module () (ConsAnn (StackType ())) (StackType ()) -> Doc ann
prettyFancyModule = prettyTypedModule . fmap (first consTy)

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

sizeStack :: [KempeTy a] -> Int64
sizeStack = getSum . foldMap (Sum . size)
