{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception    (Exception, throwIO)
import qualified Data.ByteString.Lazy as BSL
import           Kempe.AST
import           Kempe.File
import           Kempe.Lexer
import           Kempe.Monomorphize
import           Kempe.Parser
import           Kempe.Pipeline
import           Kempe.Shuttle
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
        , testGroup "type assignment"
            [ tyInfer "test/data/ty.kmp"
            , tyInfer "prelude/fn.kmp"
            , tyInfer "lib/maybe.kmp"
            , tyInfer "lib/either.kmp"
            , badType "test/err/merge.kmp" "could not unify type 'Int' with 'a_4'"
            , badType "test/err/kind.kmp" ""
            , testAssignment "test/data/ty.kmp"
            , testAssignment "lib/either.kmp"
            , tyInfer "test/data/mutual.kmp"
            , monoTest "test/data/ty.kmp"
            , pipelineWorks "test/data/ty.kmp"
            , irNoYeet "test/data/export.kmp"
            ]
        ]

irNoYeet :: FilePath -> TestTree
irNoYeet fp = testCase ("Generates IR without throwing an exception (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    (i, m) <- yeetIO $ parseWithMax contents
    let res = irGen i m
    assertBool "Worked without failure" (last res `seq` True)

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

testAssignment :: FilePath -> TestTree
testAssignment fp = testCase ("Annotates " ++ fp ++ " with types") $
    assignTypes fp *> assertBool "Does not throw an exception" True

assignTypes :: FilePath -> IO (Module () (StackType ()), Int)
assignTypes fp = do
    contents <- BSL.readFile fp
    (maxU, m) <- yeetIO $ parseWithMax contents
    yeetIO $ runTypeM maxU (assignModule m)

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

monoTest :: FilePath -> TestTree
monoTest fp = testCase ("Monomorphizes " ++ fp ++ " without error") $ monoFile fp

monoFile :: FilePath -> Assertion
monoFile fp = do
    (tyM, i) <- assignTypes fp
    let res = runMonoM i (flattenModule tyM)
    assertBool "Doesn't throw any exceptions" (res `seq` True)

pipelineWorks :: FilePath -> TestTree
pipelineWorks fp = testCase ("Functions in " ++ fp ++ " can be specialized") $ do
    contents <- BSL.readFile fp
    (maxU, m) <- yeetIO $ parseWithMax contents
    let res = monomorphize maxU m
    case res of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail type-checkign" True
