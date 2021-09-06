{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend AST
module Kempe.AST ( ConsAnn (..)
                 , Atom (..)
                 , BuiltinFn (..)
                 , KempeDecl (..)
                 , Pattern (..)
                 , Declarations
                 , Module (..)
                 , ABI (..)
                 , BuiltinTy (..)
                 , KempeTy (..)
                 , StackType (..)
                 , MonoStackType
                 , prettyMonoStackType
                 , freeVars
                 , prettyTyped
                 , prettyTypedModule
                 , prettyFancyModule
                 , prettyModule
                 , flipStackType
                 , prettyTypedDecl
                 -- * I resent this...
                 , voidStackType
                 ) where

import           Control.DeepSeq         (NFData)
import           Data.Bifunctor          (Bifunctor (..))
import qualified Data.ByteString.Lazy    as BSL
import           Data.Foldable           (toList)
import           Data.Functor            (void)
import           Data.Int                (Int64, Int8)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.List.NonEmpty      as NE
import           Data.Semigroup          ((<>))
import qualified Data.Set                as S
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Word               (Word8)
import           GHC.Generics            (Generic)
import           Kempe.AST.Size
import           Kempe.Name
import           Numeric.Natural
import           Prettyprinter           (Doc, Pretty (pretty), align, braces, brackets, colon, concatWith, dquotes, fillSep, hsep, parens, pipe, sep, vsep, (<+>))
import           Prettyprinter.Ext


-- | Annotation carried on constructors to keep size information through the IR
-- generation phase.
data ConsAnn a = ConsAnn { tySz :: Int64, tag :: Word8, consTy :: a }
    deriving (Functor, Foldable, Traversable, Generic, NFData)

instance Pretty a => Pretty (ConsAnn a) where
    pretty (ConsAnn tSz b ty) = braces ("tySz" <+> colon <+> pretty tSz <+> "tag" <+> colon <+> pretty b <+> "type" <+> colon <+> pretty ty)

voidStackType :: StackType a -> StackType ()
voidStackType (StackType vars ins outs) = StackType (S.map void vars) (void <$> ins) (void <$> outs)

data Pattern c b = PatternInt b Integer
                 | PatternCons { patternKind :: c, patternName :: TyName c } -- a constructed pattern
                 | PatternWildcard b
                 | PatternBool b Bool
                 deriving (Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor Pattern where
    second = fmap
    first f (PatternCons l tn)  = PatternCons (f l) (fmap f tn)
    first _ (PatternInt l i)    = PatternInt l i
    first _ (PatternWildcard l) = PatternWildcard l
    first _ (PatternBool l b)   = PatternBool l b

instance Pretty (Pattern c a) where
    pretty (PatternInt _ i)   = pretty i
    pretty (PatternBool _ b)  = pretty b
    pretty PatternWildcard{}  = "_"
    pretty (PatternCons _ tn) = pretty tn

prettyTypedPattern :: (Pretty a) => Pattern a b -> Doc ann
prettyTypedPattern (PatternCons ty tn) = parens (pretty tn <+> ":" <+> pretty ty)
prettyTypedPattern p                   = pretty p

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
    pretty (Case _ ls)     = "case" <+> braces (align (vsep (toList $ fmap (uncurry prettyLeaf) ls)))

prettyLeaf :: Pattern c a -> [Atom c a] -> Doc ann
prettyLeaf p as = pipe <+> pretty p <+> "->" <+> align (fillSep (fmap pretty as))

prettyTypedLeaf :: (Pretty a, Pretty b) => Pattern a b -> [Atom a b] -> Doc ann
prettyTypedLeaf p as = pipe <+> prettyTypedPattern p <+> "->" <+> align (fillSep (fmap prettyTyped as))

prettyTyped :: (Pretty a, Pretty b) => Atom a b -> Doc ann
prettyTyped (AtName ty n)    = parens (pretty n <+> ":" <+> pretty ty)
prettyTyped (Dip _ as)       = "dip(" <> fillSep (prettyTyped <$> as) <> ")"
prettyTyped (AtBuiltin ty b) = parens (pretty b <+> ":" <+> pretty ty)
prettyTyped (AtCons ty tn)   = parens (pretty tn <+> ":" <+> pretty ty)
prettyTyped (If _ as as')    = "if(" <> fillSep (prettyTyped <$> as) <> ", " <> fillSep (prettyTyped <$> as') <> ")"
prettyTyped (IntLit _ i)     = pretty i
prettyTyped (BoolLit _ b)    = pretty b
prettyTyped (Int8Lit _ i)    = pretty i <> "i8"
prettyTyped (WordLit _ n)    = pretty n <> "u"
prettyTyped (Case _ ls)      = braces ("case" <+> vsep (toList $ fmap (uncurry prettyTypedLeaf) ls))

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
              | Quot b [Atom c b]
              deriving (Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

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
    first f (Quot l as)     = Quot l (fmap (first f) as)

data BuiltinFn = Drop
               | Swap
               | Dup
               | Apply
               | IntPlus
               | IntMinus
               | IntTimes
               | IntDiv
               | IntMod
               | IntEq
               | IntLeq
               | IntLt
               | IntGeq
               | IntGt
               | IntNeq
               | IntShiftR
               | IntShiftL
               | IntXor
               | WordPlus
               | WordTimes
               | WordMinus
               | WordDiv
               | WordMod
               | WordShiftR
               | WordShiftL
               | WordXor
               | And
               | Or
               | Xor
               | IntNeg
               | Popcount
               deriving (Eq, Ord, Generic, NFData)

instance Pretty BuiltinFn where
    pretty Drop       = "drop"
    pretty Swap       = "swap"
    pretty Dup        = "dup"
    pretty Apply      = "apply"
    pretty IntPlus    = "+"
    pretty IntMinus   = "-"
    pretty IntTimes   = "*"
    pretty IntDiv     = "/"
    pretty IntMod     = "%"
    pretty IntEq      = "="
    pretty IntLeq     = "<="
    pretty IntLt      = "<"
    pretty IntShiftR  = ">>"
    pretty IntShiftL  = "<<"
    pretty WordPlus   = "+~"
    pretty WordTimes  = "*~"
    pretty WordShiftL = "<<~"
    pretty WordShiftR = ">>~"
    pretty IntXor     = "xori"
    pretty WordXor    = "xoru"
    pretty IntGeq     = ">="
    pretty IntGt      = ">"
    pretty IntNeq     = "!="
    pretty WordMinus  = "-~"
    pretty WordDiv    = "/~"
    pretty WordMod    = "%~"
    pretty And        = "&"
    pretty Or         = "||"
    pretty Xor        = "xor"
    pretty IntNeg     = "~"
    pretty Popcount   = "popcount"

prettyKempeDecl :: (Atom c b -> Doc ann) -> KempeDecl a c b -> Doc ann
prettyKempeDecl atomizer (FunDecl _ n is os as) = pretty n <+> align (":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <#> "=:" <+> brackets (align (fillSep (atomizer <$> as))))
prettyKempeDecl _ (Export _ abi n)              = "%foreign" <+> pretty abi <+> pretty n
prettyKempeDecl _ (ExtFnDecl _ n is os b)       = pretty n <+> align (":" <+> sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os) <#> "=:" <+> "$cfun" <> dquotes (pretty (decodeUtf8 b)))
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
                     deriving (Eq, Ord, Generic, NFData, Functor, Foldable, Traversable)

instance Bifunctor (KempeDecl a) where
    first _ (TyDecl x tn ns ls)        = TyDecl x tn ns ls
    first f (FunDecl l n tys tys' as)  = FunDecl l n tys tys' (fmap (first f) as)
    first _ (ExtFnDecl l n tys tys' b) = ExtFnDecl l n tys tys' b
    first _ (Export l abi n)           = Export l abi n
    second = fmap

prettyDeclarationsGeneral :: (Atom c b -> Doc ann) -> Declarations a c b -> Doc ann
prettyDeclarationsGeneral atomizer = sepDecls . fmap (prettyKempeDecl atomizer)

prettyImport :: BSL.ByteString -> Doc ann
prettyImport b = "import" <+> dquotes (pretty (decodeUtf8 b))

prettyModuleGeneral :: (Atom c b -> Doc ann) -> Module a c b -> Doc ann
prettyModuleGeneral atomizer (Module [] ds) = prettyDeclarationsGeneral atomizer ds
prettyModuleGeneral atomizer (Module is ds) = prettyLines (fmap prettyImport is) <##> prettyDeclarationsGeneral atomizer ds

prettyDecls :: Declarations a c b -> Doc ann
prettyDecls = prettyDeclarationsGeneral pretty

prettyFancyModule :: (Pretty a, Pretty b) => Declarations c (ConsAnn a) b -> Doc ann
prettyFancyModule = prettyTypedModule . fmap (first consTy)

prettyTypedDecl :: (Pretty a, Pretty b) => KempeDecl c a b -> Doc ann
prettyTypedDecl = prettyKempeDecl prettyTyped

prettyTypedModule :: (Pretty a, Pretty b) => Declarations c a b -> Doc ann
prettyTypedModule = prettyDeclarationsGeneral prettyTyped

prettyModule :: Module a c b -> Doc ann
prettyModule = prettyModuleGeneral pretty

type Declarations a c b = [KempeDecl a c b]

data Module a c b = Module { importFps :: [BSL.ByteString]
                           , body      :: [KempeDecl a c b]
                           } deriving (Generic, NFData)

extrVars :: KempeTy a -> [Name a]
extrVars TyBuiltin{}      = []
extrVars TyNamed{}        = []
extrVars (TyVar _ n)      = [n]
extrVars (TyApp _ ty ty') = extrVars ty ++ extrVars ty'

freeVars :: [KempeTy a] -> S.Set (Name a)
freeVars tys = S.fromList (concatMap extrVars tys)

-- | Used in "Kempe.Monomorphize" for patterns
flipStackType :: StackType () -> StackType ()
flipStackType (StackType vars is os) = StackType vars os is
