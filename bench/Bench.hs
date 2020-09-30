module Main (main) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Kempe.Lexer
import           Kempe.Parser

main :: IO ()
main =
    defaultMain [ env (BSL.readFile "test/data/lex.kmp") $ \contents ->
                  bgroup "parser"
                      [ bench "lex"   $ nf lexKempe contents
                      , bench "parse" $ nf parse contents
                      ]
                ]
