{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import           Kempe.File
import           Kempe.Lexer
import           Kempe.Parser
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
        , testGroup "type assignment"
            [ tyInfer "test/data/ty.kmp"
            , tyInfer "prelude/fn.kmp"
            , badType "test/err/merge.kmp" "could not unify type 'Int' with 'a_3'"
            , badType "test/err/kind.kmp" ""
            ]
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
    res <- tcFile fp
    case res of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail type-checkign" True

badType :: FilePath -> String -> TestTree
badType fp msg = testCase ("Detects error (" ++ fp ++ ")") $ do
    res <- tcFile fp
    case res of
        Left err -> show (pretty err) @?= msg
        Right{}  -> assertFailure "No error detected!"
