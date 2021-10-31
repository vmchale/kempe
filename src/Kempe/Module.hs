-- | Pretty easy since doesn't need renaming.
--
-- Just thread lexer state through, remove duplicates.
module Kempe.Module ( parseProcess
                    ) where

import           Control.Exception          (Exception, throwIO)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import qualified Data.Set                   as S
import           Data.Tuple.Ext             (fst3, third3)
import           Kempe.AST
import           Kempe.Lexer
import           Kempe.Parser


parseProcess :: FilePath -> IO (Int, Declarations AlexPosn AlexPosn AlexPosn)
parseProcess fp = do
    (st, [], ds) <- loopFps True [fp] alexInitUserState
    pure (fst3 st, {-# SCC "dedup" #-} dedup ds)

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

loopFps :: Bool -> [FilePath] -> AlexUserState -> IO (AlexUserState, [FilePath], Declarations AlexPosn AlexPosn AlexPosn)
loopFps _ [] st = pure (st, [], [])
loopFps isInit (fp:fps) st = do
    (st', Module is ds) <- parseStep fp st
    let discardDs = if isInit then id else filter (not . isExport)
    third3 (++ discardDs ds) <$> loopFps False (fmap ASCII.unpack (reverse is) ++ fps) st'
    where isExport Export{} = True
          isExport _        = False

parseStep :: FilePath -> AlexUserState -> IO (AlexUserState, Module AlexPosn AlexPosn AlexPosn)
parseStep fp st = do
    contents <- BSL.readFile fp
    yeetIO $ parseWithCtx contents st

dedup :: Ord a => [a] -> [a]
dedup = loop S.empty
    where loop _ [] = []
          loop acc (x:xs) =
            if S.member x acc
                then loop acc xs
                else x : loop (S.insert x acc) xs
