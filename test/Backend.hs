module Backend ( backendTests
               ) where

import           Control.DeepSeq           (deepseq)
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Liveness
import           Kempe.File
import           Kempe.Inline
import           Kempe.Monomorphize
import           Kempe.Pipeline
import           Kempe.Shuttle
import           Prettyprinter             (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Type

backendTests :: TestTree
backendTests =
    testGroup "Backend-ish"
        [ monoTest "test/data/ty.kmp"
        , inlineTest "lib/numbertheory.kmp"
        , inlineTest "examples/factorial.kmp"
        , pipelineWorks "test/data/ty.kmp"
        , pipelineWorks "examples/splitmix.kmp"
        , pipelineWorks "examples/factorial.kmp"
        , pipelineWorks "test/data/mutual.kmp"
        , irNoYeet "test/data/export.kmp"
        , irNoYeet "examples/splitmix.kmp"
        , irNoYeet "examples/factorial.kmp"
        , irNoYeet "test/data/maybeC.kmp"
        , x86NoYeet "examples/factorial.kmp"
        , x86NoYeet "examples/splitmix.kmp"
        , controlFlowGraph "examples/factorial.kmp"
        , controlFlowGraph "examples/splitmix.kmp"
        , liveness "examples/factorial.kmp"
        , liveness "examples/splitmix.kmp"
        , codegen "examples/factorial.kmp"
        , codegen "examples/splitmix.kmp"
        , codegen "lib/numbertheory.kmp"
        ]

codegen :: FilePath -> TestTree
codegen fp = testCase ("Generates code without throwing an exception (" ++ fp ++ ")") $ do
    parsed <- parsedFp fp
    let code = uncurry x86Alloc parsed
    assertBool "Doesn't fail" (code `deepseq` True)

liveness :: FilePath -> TestTree
liveness fp = testCase ("Liveness analysis terminates (" ++ fp ++ ")") $ do
    parsed <- parsedFp fp
    let x86 = uncurry x86Parsed parsed
        cf = mkControlFlow x86
    assertBool "Doesn't bottom" (reconstruct cf `deepseq` True)

controlFlowGraph :: FilePath -> TestTree
controlFlowGraph fp = testCase ("Doesn't crash while creating control flow graph for " ++ fp) $ do
    parsed <- parsedFp fp
    let x86 = uncurry x86Parsed parsed
    assertBool "Worked without exception" (mkControlFlow x86 `deepseq` True)

x86NoYeet :: FilePath -> TestTree
x86NoYeet fp = testCase ("Selects instructions for " ++ fp) $ do
    parsed <- parsedFp fp
    let x86 = uncurry x86Parsed parsed
    assertBool "Worked without exception" (x86 `deepseq` True)

irNoYeet :: FilePath -> TestTree
irNoYeet fp = testCase ("Generates IR without throwing an exception (" ++ fp ++ ")") $ do
    (i, m) <- parsedFp fp
    let res = fst $ irGen i m
    assertBool "Worked without failure" (res `deepseq` True)

inlineTest :: FilePath -> TestTree
inlineTest fp = testCase ("Inlines " ++ fp ++ " without error") $ inlineFile fp

inlineFile :: FilePath -> Assertion
inlineFile fp = do
    (_, m) <- parsedFp fp
    let res = inline m
    assertBool "Doesn't bottom when inlining" (res `deepseq` True)

monoTest :: FilePath -> TestTree
monoTest fp = testCase ("Monomorphizes " ++ fp ++ " without error") $ monoFile fp

monoFile :: FilePath -> Assertion
monoFile fp = do
    (tyM, i) <- assignTypes fp
    let res = runMonoM i (flattenModule tyM)
    assertBool "Doesn't throw any exceptions" (res `deepseq` True)

pipelineWorks :: FilePath -> TestTree
pipelineWorks fp = testCase ("Functions in " ++ fp ++ " can be specialized") $ do
    (maxU, m) <- parsedFp fp
    let res = monomorphize maxU m
    case res of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail type-checking" True
