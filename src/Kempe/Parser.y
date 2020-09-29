{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Kempe.Parser ( parse
                        , ParseError (..)
                        ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
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
    vbar { TokSym $$ VBar }
    caseArr { TokSym $$ CaseArr }

    name { TokName _ $$ }
    tyName { TokTyName  _ $$ }

    type { TokKeyword $$ KwType } 

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p q p { $3 : [$1] }

braces(p)
    : lbrace p rbrace { $2 }

Module :: { Module AlexPosn }
       : many(Decl) { $1 }

Decl :: { KempeDecl AlexPosn }
     : TyDecl { KempeTyDecl $1 }

TyDecl :: { TyDecl AlexPosn }
       : type tyName many(name) braces(sepBy(TyLeaf, vbar)) { TyDecl $1 $2 (reverse $3) (reverse $4) }
       | type tyName many(name) lbrace rbrace { TyDecl $1 $2 (reverse $3) [] }

Type :: { KempeTy AlexPosn }
     : name { TyVar (Name.loc $1) $1 }
     | tyName { TyNamed (Name.loc $1) $1 }

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
parse = runParse parseModule

runParse :: Parse a -> BSL.ByteString -> Either (ParseError AlexPosn) a
runParse parser str = liftErr $ runAlex str (runExceptT parser)

liftErr :: Either String (Either (ParseError a) b) -> Either (ParseError a) b
liftErr (Left err)         = Left (LexErr err)
liftErr (Right (Left err)) = Left err
liftErr (Right (Right x))  = Right x

}
