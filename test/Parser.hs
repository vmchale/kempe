module Parser ( parserTests
              ) where

import qualified Data.ByteString.Lazy as BSL
import           Kempe.Lexer
import           Kempe.Parser
import           Prettyprinter        (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit

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


parserTests :: TestTree
parserTests =
    testGroup "Parser golden tests"
        [ lexNoError "test/data/lex.kmp"
        , lexNoError "examples/splitmix.kmp"
        , parseNoError "test/data/lex.kmp"
        , parseNoError "examples/splitmix.kmp"
        , parseNoError "prelude/num.kmp"
        ]
