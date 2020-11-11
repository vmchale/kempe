{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.DeepSeq           (deepseq)
import           Control.Exception         (Exception, throwIO)
import qualified Data.ByteString.Lazy      as BSL
import           Kempe.AST
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Liveness
import           Kempe.File
import           Kempe.Lexer
import           Kempe.Monomorphize
import           Kempe.Parser
import           Kempe.Pipeline
import           Kempe.Shuttle
import           Kempe.TyAssign
import           Prettyprinter             (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Kempe compiler tests"
        [ testGroup "Parser golden tests"
            [ lexNoError "test/data/lex.kmp"
            , lexNoError "examples/splitmix.kmp"
            , parseNoError "test/data/lex.kmp"
            , parseNoError "examples/splitmix.kmp"
            ]
        , testGroup "type assignment"
            [ tyInfer "test/data/ty.kmp"
            , tyInfer "prelude/fn.kmp"
            , tyInfer "lib/maybe.kmp"
            , tyInfer "lib/either.kmp"
            , tyInfer "test/data/mutual.kmp"
            , tyInfer "examples/factorial.kmp"
            , badType "test/err/merge.kmp" "Type a_4 -- a_4 Int is not as general as type a_3 -- a_3 a_3"
            , badType "test/err/kind.kmp" ""
            , testAssignment "test/data/ty.kmp"
            , testAssignment "lib/either.kmp"
            , testAssignment "prelude/fn.kmp"
            , testAssignment "test/data/mutual.kmp"
            , monoTest "test/data/ty.kmp"
            , pipelineWorks "test/data/ty.kmp"
            , pipelineWorks "examples/splitmix.kmp"
            , pipelineWorks "examples/factorial.kmp"
            , pipelineWorks "test/data/mutual.kmp"
            , irNoYeet "test/data/export.kmp"
            , irNoYeet "examples/splitmix.kmp"
            , irNoYeet "examples/factorial.kmp"
            , x86NoYeet "examples/factorial.kmp"
            , controlFlowGraph "examples/factorial.kmp"
            , liveness "examples/factorial.kmp"
            , codegen "examples/factorial.kmp"
            ]
        ]

codegen :: FilePath -> TestTree
codegen fp = testCase ("Generates code without throwing an exception (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    parsed <- yeetIO $ parseWithMax contents
    let liveX86 = reconstruct $ mkControlFlow $ uncurry x86Parsed parsed
    assertBool "Doesn't fail" (allocRegs liveX86 `deepseq` True)

liveness :: FilePath -> TestTree
liveness fp = testCase ("Liveness analysis terminates (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    parsed <- yeetIO $ parseWithMax contents
    let x86 = uncurry x86Parsed parsed
        cf = mkControlFlow x86
    assertBool "Doesn't bottom" (reconstruct cf `deepseq` True)

controlFlowGraph :: FilePath -> TestTree
controlFlowGraph fp = testCase ("Doesn't crash while creating control flow graph for " ++ fp) $ do
    contents <- BSL.readFile fp
    parsed <- yeetIO $ parseWithMax contents
    let x86 = uncurry x86Parsed parsed
    assertBool "Worked without exception" (mkControlFlow x86 `deepseq` True)

x86NoYeet :: FilePath -> TestTree
x86NoYeet fp = testCase ("Selects instructions for " ++ fp) $ do
    contents <- BSL.readFile fp
    parsed <- yeetIO $ parseWithMax contents
    let x86 = uncurry x86Parsed parsed
    assertBool "Worked without exception" (x86 `deepseq` True)

irNoYeet :: FilePath -> TestTree
irNoYeet fp = testCase ("Generates IR without throwing an exception (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    (i, m) <- yeetIO $ parseWithMax contents
    let res = fst $ irGen i m
    assertBool "Worked without failure" (res `deepseq` True)

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
        Right{}  -> assertBool "Doesn't fail type-checking" True

badType :: FilePath -> String -> TestTree
badType fp msg = testCase ("Detects error (" ++ fp ++ ")") $ do
    res <- tcFile fp
    case res of
        Left err -> show (pretty err) @?= msg
        Right{}  -> assertFailure "No error detected!"

testAssignment :: FilePath -> TestTree
testAssignment fp = testCase ("Annotates " ++ fp ++ " with types") $ do
    (m, i) <- assignTypes fp
    assertBool "Does not throw an exception" (m `deepseq` i `seq` True)

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
    assertBool "Doesn't throw any exceptions" (res `deepseq` True)

pipelineWorks :: FilePath -> TestTree
pipelineWorks fp = testCase ("Functions in " ++ fp ++ " can be specialized") $ do
    contents <- BSL.readFile fp
    (maxU, m) <- yeetIO $ parseWithMax contents
    let res = monomorphize maxU m
    case res of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail type-checking" True
