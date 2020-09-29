{
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module Kempe.Lexer ( alexMonadScan
                       , runAlex
                       , lexKempe
                       , AlexPosn (..)
                       , Alex (..)
                       , Token (..)
                       , Keyword (..)
                       , Sym (..)
                       ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.Functor (($>))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Kempe.Name
import Kempe.Unique
import Prettyprinter (Pretty (pretty), colon)

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

$latin = [a-zA-Z]

@follow_char = [$latin $digit \-\!\_]

@name = [a-z] @follow_char*
@tyname = [A-Z] @follow_char*

tokens :-

    <0> {

        $white+                  ;

        ";".*                    ; -- comment

        "--"                     { mkSym Arrow }
        "=:"                     { mkSym DefEq }
        ":"                      { mkSym Colon }
        "{"                      { mkSym LBrace }
        "}"                      { mkSym RBrace }
        "["                      { mkSym LSqBracket }
        "]"                      { mkSym RSqBracket }
        \|                       { mkSym VBar }
        "->"                     { mkSym CaseArr }

        type                     { mkKw KwType }
        import                   { mkKw KwImport }
        case                     { mkKw KwCase }

        $digit+                  { tok (\p s -> alex $ TokInt p (read $ ASCII.unpack s)) }

        @name                    { tok (\p s -> TokName p <$> newIdentAlex p (mkText s)) }
        @tyname                  { tok (\p s -> TokTyName p <$> newIdentAlex p (mkText s)) }

    }

{

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkKw = constructor TokKeyword

mkSym = constructor TokSym

mkText :: BSL.ByteString -> T.Text
mkText = decodeUtf8 . BSL.toStrict

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

-- functional bimap?
type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Name AlexPosn))

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_ust :: Alex AlexUserState
get_ust = gets_alex alex_ust

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

set_ust :: AlexUserState -> Alex ()
set_ust st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = st }

alexEOF = EOF <$> get_pos

data Sym = Arrow
         | Plus
         | Minus
         | Percent
         | Div
         | Times
         | DefEq
         | Eq
         | Colon
         | LBrace
         | RBrace
         | Semicolon
         | LSqBracket
         | RSqBracket
         | VBar
         | CaseArr
         deriving (Generic, NFData)

instance Pretty Sym where
    pretty Arrow      = "--"
    pretty Plus       = "+"
    pretty Minus      = "-"
    pretty Percent    = "%"
    pretty Div        = "/"
    pretty Times      = "*"
    pretty DefEq      = "        = :"
    pretty Eq         = "        = "
    pretty Colon      = ":"
    pretty LBrace     = "{"
    pretty RBrace     = "}"
    pretty Semicolon  = ";"
    pretty LSqBracket = "["
    pretty RSqBracket = "]"
    pretty VBar       = "|"
    pretty CaseArr    = "->"

data Keyword = KwType
             | KwImport
             | KwCase
             deriving (Generic, NFData)

instance Pretty Keyword where
    pretty KwType   = "type"
    pretty KwImport = "import"
    pretty KwCase   = "case"

data Token a = EOF { loc :: a }
             | TokSym { loc :: a, _sym :: Sym }
             | TokName { loc :: a, _name :: (Name a) }
             | TokTyName { loc :: a, _tyName :: (TyName a) }
             | TokKeyword { loc :: a, _kw :: Keyword }
             | TokInt { loc :: a, int :: Integer }
             deriving (Generic, NFData)

instance Pretty (Token a) where
    pretty EOF{}             = ""
    pretty (TokSym _ s)      = pretty s
    pretty (TokName _ n)     = pretty n
    pretty (TokTyName _ tn)  = pretty tn
    pretty (TokKeyword _ kw) = pretty kw
    pretty (TokInt _ i)      = pretty i

newIdentAlex :: AlexPosn -> T.Text -> Alex (Name AlexPosn)
newIdentAlex pos t = do
    st <- get_ust
    let (st', n) = newIdent pos t st
    set_ust st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Name AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Name t (Unique i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name t (Unique i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)

loop :: Alex [Token AlexPosn]
loop = do
    tok' <- alexMonadScan
    case tok' of
        EOF{} -> pure []
        _ -> (tok' :) <$> loop

lexKempe :: BSL.ByteString -> Either String [Token AlexPosn]
lexKempe = flip runAlex loop

}
