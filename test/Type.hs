module Type ( typeTests
            , assignTypes
            , yeetIO
            ) where

import           Control.DeepSeq   (deepseq)
import           Control.Exception (Exception, throwIO)
import           Kempe.AST
import           Kempe.File
import           Kempe.TyAssign
import           Prettyprinter     (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit

typeTests :: TestTree
typeTests =
    testGroup "Type assignment"
        [ tyInfer "test/data/ty.kmp"
        , tyInfer "prelude/fn.kmp"
        , tyInfer "lib/maybe.kmp"
        , tyInfer "lib/either.kmp"
        , tyInfer "test/data/mutual.kmp"
        , tyInfer "examples/factorial.kmp"
        , tyInfer "lib/bool.kmp"
        , tyInfer "examples/hamming.kmp"
        , tyInfer "lib/gaussian.kmp"
        , tyInfer "lib/tuple.kmp"
        , badType "test/err/merge.kmp" "Type a_5 -- Int a_4 is not as general as type a_3 -- a_3 a_3"
        , badType "test/err/kind.kmp" "Ill-kinded type: '(Maybe_1 Maybe_1)'. Note that type variables have kind \11089 in Kempe."
        , badType "test/err/patternMatch.kmp" "Inexhaustive pattern match."
        , testAssignment "test/data/ty.kmp"
        , testAssignment "lib/either.kmp"
        , testAssignment "prelude/fn.kmp"
        , testAssignment "test/data/mutual.kmp"
        ]

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

assignTypes :: FilePath -> IO (Module () (StackType ()) (StackType ()), Int)
assignTypes fp = do
    (maxU, m) <- parsedFp fp
    yeetIO $ runTypeM maxU (assignModule m)

testAssignment :: FilePath -> TestTree
testAssignment fp = testCase ("Annotates " ++ fp ++ " with types") $ do
    (m, i) <- assignTypes fp
    assertBool "Does not throw an exception" (m `deepseq` i `seq` True)

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
