{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Kempe.Parser ( parseTyDecl
                        , ParseError (..)
                        ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kempe.AST
import Kempe.Lexer
import Kempe.Name hiding (loc)
import Prettyprinter (Pretty (pretty), (<+>))

}

%name parseTyDecl TyDecl
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

TyDecl :: { TyDecl AlexPosn }
       : type tyName many(name) braces(sepBy(TyLeaf, vbar)) { TyDecl $1 $2 (reverse $3) (reverse $4) }

TyLeaf :: { (Name AlexPosn, [TyName AlexPosn]) }
       : name many(tyName) { ($1, reverse $2) }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  deriving (Generic, NFData)

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok)  = pretty (loc tok) <+> "Unexpected" <+> pretty tok
    pretty (LexErr str)      = pretty (T.pack str)

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

}
