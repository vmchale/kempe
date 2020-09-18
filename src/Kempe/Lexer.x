{
    module Kempe.Lexer ( alexMonadScan
                       , runAlex
                       , loop
                       , AlexPosn (..)
                       , Alex (..)
                       ) where

import Control.Arrow ((&&&))
import Data.Functor (($>))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Text as T
import Kempe.Name
import Kempe.Unique

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

$latin = [a-zA-Z]

tokens :-

    <0> ";".*                    ; -- comment

{

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkSym = constructor TokSym

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

data Token a = EOF a
             | TokSym a Sym

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

}
