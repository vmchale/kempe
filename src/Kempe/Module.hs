-- | Pretty easy since doesn't need renaming.
--
-- Just thread lexer state through, remove duplicates.
module Kempe.Module ( parseProcess
                    ) where

import           Control.Arrow        ((***))
import           Control.Exception    (Exception, throwIO)
import qualified Data.ByteString.Lazy as BSL
import           Data.Tuple.Extra     (fst3)
import           Kempe.AST
import           Kempe.Lexer
import           Kempe.Parser

parseProcess :: FilePath -> IO (Int, Declarations AlexPosn AlexPosn AlexPosn)
parseProcess fp = do
    contents <- BSL.readFile fp
    (st, m) <- yeetIO $ parseWithInitCtx contents
    pure $ (fst3 *** body) (st, m)

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

parseStep :: FilePath -> AlexUserState -> IO (AlexUserState, Module AlexPosn AlexPosn AlexPosn)
parseStep fp st = do
    contents <- BSL.readFile fp
    yeetIO $ parseWithCtx contents st
