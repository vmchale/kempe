{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Kempe.Parser ( parse 
                        , parseWithMax
                        , ParseError (..)
                        ) where

import Control.Composition ((.*))
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kempe.AST
import Kempe.Lexer
import qualified Kempe.Name as Name
import Kempe.Name hiding (loc)
import Prettyprinter (Pretty (pretty), (<+>))

import Debug.Trace (traceShow)

}

%name parseModule Module
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    arrow { TokSym $$ Arrow }
    defEq { TokSym $$ DefEq }
    colon { TokSym $$ Colon }
    lbrace { TokSym $$ LBrace }
    rbrace { TokSym $$ RBrace }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }
    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    vbar { TokSym $$ VBar }
    caseArr { TokSym $$ CaseArr }
    comma { TokSym $$ Comma }
    underscore { TokSym $$ Underscore }

    name { TokName _ $$ }
    tyName { TokTyName  _ $$ }
    foreign { TokForeign _ $$ }

    intLit { $$@(TokInt _ _) }

    type { TokKeyword $$ KwType }
    case { TokKeyword $$ KwCase }
    cfun { TokKeyword $$ KwCfun }
    if { TokKeyword $$ KwIf }

    dip { TokBuiltin $$ BuiltinDip }
    boolLit { $$@(TokBuiltin _ (BuiltinBoolLit _)) }
    bool { TokBuiltin $$ BuiltinBool }
    int { TokBuiltin $$ BuiltinInt }
    ptr { TokBuiltin $$ BuiltinPtr }
    dup { TokBuiltin $$ BuiltinDup }
    swap { TokBuiltin $$ BuiltinSwap }
    drop { TokBuiltin $$ BuiltinDrop }

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

some(p)
    : many(p) p { $2 :| $1 }

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p q p { $3 : [$1] }

braces(p)
    : lbrace p rbrace { $2 }

brackets(p)
    : lsqbracket p rsqbracket { $2 }

parens(p)
    : lparen p rparen { $2 }

Module :: { Module AlexPosn }
       : many(Decl) { (reverse $1) }

Decl :: { KempeDecl AlexPosn }
     : TyDecl { $1 }
     | FunDecl { $1 }

TyDecl :: { KempeDecl AlexPosn }
       : type tyName many(name) braces(sepBy(TyLeaf, vbar)) { TyDecl $1 $2 (reverse $3) (reverse $4) }
       | type tyName many(name) lbrace rbrace { TyDecl $1 $2 (reverse $3) [] } -- necessary since sepBy always has some "flesh"

Type :: { KempeTy AlexPosn }
     : name { TyVar (Name.loc $1) $1 }
     | tyName { TyNamed (Name.loc $1) $1 }
     | bool { TyBuiltin $1 TyBool }
     | int { TyBuiltin $1 TyInt }
     | ptr { TyBuiltin $1 TyPtr }

FunDecl :: { KempeDecl AlexPosn }
        : FunSig FunBody { uncurry4 FunDecl $1 $2 }

FunSig :: { (AlexPosn, Name AlexPosn, [KempeTy AlexPosn], [KempeTy AlexPosn]) }
       : name colon many(Type) arrow many(Type) { ($2, $1, $3, $5) }

FunBody :: { [Atom AlexPosn] }
        : defEq brackets(many(Atom)) { $2 }

Atom :: { Atom AlexPosn }
     : name { AtName (Name.loc $1) $1 }
     | tyName { AtCons (Name.loc $1) $1 }
     | lbrace case some(CaseLeaf) rbrace { Case $2 (NE.reverse $3) }
     | intLit { IntLit (loc $1) (int $1) }
     | cfun foreign { Ccall $1 $2 }
     | dip parens(many(Atom)) { Dip $1 $2 }
     | if lparen many(Atom) comma many(Atom) rparen { If $1 $3 $5 }
     | boolLit { BoolLit (loc $1) (bool $ builtin $1) }
     | dup { AtBuiltin $1 Dup }
     | drop { AtBuiltin $1 Drop }
     | swap { AtBuiltin $1 Swap }

CaseLeaf :: { (Pattern AlexPosn, [Atom AlexPosn]) }
         : vbar Pattern caseArr many(Atom) { ($2, reverse $4) }

Pattern :: { Pattern AlexPosn }
        : tyName many(Pattern) { PatternCons (Name.loc $1) $1 $2 }
        | name { PatternVar (Name.loc $1) $1 }
        | underscore { PatternWildcard $1 }
        | intLit { PatternInt (loc $1) (int $1) }
        | boolLit { PatternBool (loc $1) (bool $ builtin $1) }

-- FIXME: tyName is uppercase, need "free" variables as well...
TyLeaf :: { (Name AlexPosn, [KempeTy AlexPosn]) }
       : tyName many(Type) { ($1, reverse $2) }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  | NoImpl (Name a)
                  deriving (Generic, NFData)

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok)  = pretty (loc tok) <+> "Unexpected" <+> pretty tok
    pretty (LexErr str)      = pretty (T.pack str)
    pretty (NoImpl n)        = pretty (Name.loc n) <+> "Signature for" <+> pretty n <+> "is not accompanied by an implementation"

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Module AlexPosn)
parse = fmap snd . parseWithMax

parseWithMax :: BSL.ByteString -> Either (ParseError AlexPosn) (Int, Module AlexPosn)
parseWithMax = fmap (first fst3) . runParse parseModule

runParse :: Parse a -> BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, a)
runParse parser str = liftErr $ runAlexSt str (runExceptT parser)

liftErr :: Either String (b, Either (ParseError a) c) -> Either (ParseError a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(x, y, z, w) = f x y z w

fst3 :: (a, b, c) -> a
fst3 ~(x,_,_) = x

}
