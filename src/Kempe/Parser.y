{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Kempe.Parser ( parse
                        , parseWithMax
                        , parseWithCtx
                        , parseWithInitCtx
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
import Data.Tuple.Extra (fst3)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kempe.AST
import Kempe.Lexer
import qualified Kempe.Name as Name
import Kempe.Name hiding (loc)
import Prettyprinter (Pretty (pretty), (<+>))

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

    plus { TokSym $$ Plus }
    plusU { TokSym $$ PlusU }
    minus { TokSym $$ Minus }
    times { TokSym $$ Times }
    timesU { TokSym $$ TimesU }
    div { TokSym $$ Div }
    percent { TokSym $$ Percent }
    eq { TokSym $$ Eq }
    neq { TokSym $$ Neq }
    leq { TokSym $$ Leq }
    lt { TokSym $$ Lt }
    geq { TokSym $$ Geq }
    gt { TokSym $$ Gt }
    shiftrU { TokSym $$ ShiftRU }
    shiftlU { TokSym $$ ShiftLU }
    shiftr { TokSym $$ ShiftR }
    shiftl { TokSym $$ ShiftL }
    neg { TokSym $$ NegTok }
    and { TokSym $$ AndTok }
    or { TokSym $$ OrTok }

    name { TokName _ $$ }
    tyName { TokTyName  _ $$ }
    foreignName { TokForeign _ $$ }
    moduleFile { TokModuleStr _ $$ }

    intLit { $$@(TokInt _ _) }
    wordLit { $$@(TokWord _ _) }
    int8Lit { $$@(TokInt8 _ _) }

    type { TokKeyword $$ KwType }
    case { TokKeyword $$ KwCase }
    cfun { TokKeyword $$ KwCfun }
    if { TokKeyword $$ KwIf }
    foreign { TokKeyword $$ KwForeign }
    cabi { TokKeyword $$ KwCabi }
    kabi { TokKeyword $$ KwKabi }
    import { TokKeyword $$ KwImport }

    dip { TokBuiltin $$ BuiltinDip }
    boolLit { $$@(TokBuiltin _ (BuiltinBoolLit _)) }
    bool { TokBuiltin $$ BuiltinBool }
    int { TokBuiltin $$ BuiltinInt }
    int8 { TokBuiltin $$ BuiltinInt8 }
    word { TokBuiltin $$ BuiltinWord }
    dup { TokBuiltin $$ BuiltinDup }
    swap { TokBuiltin $$ BuiltinSwap }
    drop { TokBuiltin $$ BuiltinDrop }
    apply { TokBuiltin $$ BuiltinApply }
    intXor { TokBuiltin $$ BuiltinIntXor }
    wordXor { TokBuiltin $$ BuiltinWordXor }
    boolXor { TokBuiltin $$ BuiltinBoolXor }
    popcount { TokBuiltin $$ BuiltinPopcount }

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

Module :: { Module AlexPosn AlexPosn AlexPosn }
       : many(Import) Declarations { Module (reverse $1) $2 }

Declarations :: { Declarations AlexPosn AlexPosn AlexPosn }
             : many(Decl) { (reverse $1) }

Import :: { BSL.ByteString }
       : import moduleFile { $2 }

ABI :: { ABI }
    : cabi { Cabi }
    | kabi { Kabi }

Decl :: { KempeDecl AlexPosn AlexPosn AlexPosn }
     : TyDecl { $1 }
     | FunDecl { $1 }
     | foreign ABI name { Export $1 $2 $3 }

TyDecl :: { KempeDecl AlexPosn AlexPosn AlexPosn }
       : type tyName many(name) braces(sepBy(TyLeaf, vbar)) { TyDecl $1 $2 (reverse $3) (reverse $4) }
       | type tyName many(name) braces(TyLeaf) { TyDecl $1 $2 (reverse $3) [$4] }
       | type tyName many(name) lbrace rbrace { TyDecl $1 $2 (reverse $3) [] } -- necessary since sepBy always has some "flesh"

Type :: { KempeTy AlexPosn }
     : name { TyVar (Name.loc $1) $1 }
     | tyName { TyNamed (Name.loc $1) $1 }
     | bool { TyBuiltin $1 TyBool }
     | int { TyBuiltin $1 TyInt }
     | int8 { TyBuiltin $1 TyInt8 }
     | word { TyBuiltin $1 TyWord }
     | lparen Type Type rparen { TyApp $1 $2 $3 }
     | lsqbracket many(Type) arrow many(Type) rsqbracket { QuotTy $1 (reverse $2) (reverse $4) }

FunDecl :: { KempeDecl AlexPosn AlexPosn AlexPosn }
        : FunSig FunBody { uncurry4 FunDecl $1 $2 }
        | FunSig defEq cfun foreignName { uncurry4 ExtFnDecl $1 $4 }


FunSig :: { (AlexPosn, Name AlexPosn, [KempeTy AlexPosn], [KempeTy AlexPosn]) }
       : name colon many(Type) arrow many(Type) { ($2, $1, reverse $3, reverse $5) }

FunBody :: { [Atom AlexPosn AlexPosn] }
        : defEq brackets(many(Atom)) { reverse $2 }

Atom :: { Atom AlexPosn AlexPosn }
     : name { AtName (Name.loc $1) $1 }
     | tyName { AtCons (Name.loc $1) $1 }
     | lbrace case some(CaseLeaf) rbrace { Case $2 (NE.reverse $3) }
     | intLit { IntLit (loc $1) (int $1) }
     | wordLit { WordLit (loc $1) (word $1) }
     | int8Lit { Int8Lit (loc $1) (int8 $1) }
     | dip parens(many(Atom)) { Dip $1 (reverse $2) }
     | if lparen many(Atom) comma many(Atom) rparen { If $1 (reverse $3) (reverse $5) }
     | boolLit { BoolLit (loc $1) (bool $ builtin $1) }
     | lsqbracket many(Atom) rsqbracket { Quot $1 (reverse $2) }
     | dup { AtBuiltin $1 Dup }
     | drop { AtBuiltin $1 Drop }
     | swap { AtBuiltin $1 Swap }
     | apply { Apply }
     | plus { AtBuiltin $1 IntPlus }
     | plusU { AtBuiltin $1 WordPlus }
     | minus { AtBuiltin $1 IntMinus }
     | times { AtBuiltin $1 IntTimes }
     | timesU { AtBuiltin $1 WordTimes }
     | div { AtBuiltin $1 IntDiv }
     | percent { AtBuiltin $1 IntMod }
     | eq { AtBuiltin $1 IntEq }
     | neq { AtBuiltin $1 IntNeq }
     | leq { AtBuiltin $1 IntLeq }
     | lt { AtBuiltin $1 IntLt }
     | geq { AtBuiltin $1 IntGeq }
     | gt { AtBuiltin $1 IntGt }
     | and { AtBuiltin $1 And }
     | or { AtBuiltin $1 Or }
     | neg { AtBuiltin $1 IntNeg }
     | shiftl { AtBuiltin $1 IntShiftL }
     | shiftr { AtBuiltin $1 IntShiftR }
     | shiftlU { AtBuiltin $1 WordShiftL }
     | shiftrU { AtBuiltin $1 WordShiftR }
     | intXor { AtBuiltin $1 IntXor }
     | wordXor { AtBuiltin $1 WordXor }
     | boolXor { AtBuiltin $1 Xor }
     | popcount { AtBuiltin $1 Popcount }

CaseLeaf :: { (Pattern AlexPosn AlexPosn, [Atom AlexPosn AlexPosn]) }
         : vbar Pattern caseArr many(Atom) { ($2, reverse $4) }

Pattern :: { Pattern AlexPosn AlexPosn }
        : tyName { PatternCons (Name.loc $1) $1 }
        | underscore { PatternWildcard $1 }
        | intLit { PatternInt (loc $1) (int $1) }
        | boolLit { PatternBool (loc $1) (bool $ builtin $1) }

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

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Module AlexPosn AlexPosn AlexPosn)
parse = fmap snd . parseWithMax

parseWithMax :: BSL.ByteString -> Either (ParseError AlexPosn) (Int, Module AlexPosn AlexPosn AlexPosn)
parseWithMax = fmap (first fst3) . parseWithInitCtx

parseWithInitCtx :: BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, Module AlexPosn AlexPosn AlexPosn)
parseWithInitCtx bsl = parseWithCtx bsl alexInitUserState

parseWithCtx :: BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Module AlexPosn AlexPosn AlexPosn)
parseWithCtx = parseWithInitSt parseModule

runParse :: Parse a -> BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, a)
runParse parser str = liftErr $ runAlexSt str (runExceptT parser)

parseWithInitSt :: Parse a -> BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, a)
parseWithInitSt parser str st = liftErr $ withAlexSt str st (runExceptT parser)
    where liftErr (Left err)            = Left (LexErr err)
          liftErr (Right (_, Left err)) = Left err
          liftErr (Right (i, Right x))  = Right (i, x)

liftErr :: Either String (b, Either (ParseError a) c) -> Either (ParseError a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f ~(x, y, z, w) = f x y z w

}
