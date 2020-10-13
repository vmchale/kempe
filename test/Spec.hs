{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception    (throwIO)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (traverse_)
import           Kempe.Lexer
import           Kempe.Parser
import           Kempe.TyAssign
import           Prettyprinter        (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Kempe compiler tests"
        [ testGroup "Parser golden tests"
            [ lexNoError "test/data/lex.kmp"
            , parseNoError "test/data/lex.kmp"
            ]
        , tyInfer "test/data/ty.kmp"
        ]

lexNoError :: FilePath -> TestTree
lexNoError fp = testCase ("Lexing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    case lexKempe contents of
        Left err -> assertFailure err
        Right{}  -> assertBool "Doesn't fail lexing" True

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    case parse contents of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail parsing" True

tyInfer :: FilePath -> TestTree
tyInfer fp = testCase ("Checks types (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    (maxU, ds) <- either throwIO pure $ parseWithMax contents
    let res = runTypeM maxU (traverse_ tyInsert ds)
    case res of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail type-checkign" True
