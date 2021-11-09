module Parser ( parserTests
              ) where

import qualified Data.ByteString.Lazy as BSL
import           Kempe.Parser
import           Prettyprinter        (pretty)
import           Test.Tasty
import           Test.Tasty.HUnit

parseNoError :: FilePath -> TestTree
parseNoError fp = testCase ("Parsing doesn't fail (" ++ fp ++ ")") $ do
    contents <- BSL.readFile fp
    case parse contents of
        Left err -> assertFailure (show $ pretty err)
        Right{}  -> assertBool "Doesn't fail parsing" True


parserTests :: TestTree
parserTests =
    testGroup "Parser golden tests"
        [ parseNoError "test/data/lex.kmp"
        , parseNoError "examples/splitmix.kmp"
        ]
