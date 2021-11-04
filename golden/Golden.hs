module Main (main) where

import           CDecl
import           Data.Tuple.Extra (uncurry3)
import           Harness
import           System.Info      (arch)
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "Golden output tests" $
        fmap (uncurry3 goldenOutput) allGoldens ++ crossTests
            ++ fmap (uncurry goldenCDecl) headerGoldens

-- These are redundant on arm
crossTests :: [TestTree]
crossTests = case arch of
    "x86_64"  -> fmap (uncurry3 crossGolden) allGoldens
    "aarch64" -> []
    _         -> error "Test suite must be run on x86_64 or aarch64"

headerGoldens :: [(FilePath, FilePath)]
headerGoldens = [ ("test/examples/splitmix.kmp", "test/include/splitmix.h")
                , ("lib/numbertheory.kmp", "test/include/num.h")
                ]

allGoldens :: [(FilePath, FilePath, FilePath)]
allGoldens =
    [ ("examples/factorial.kmp", "test/harness/factorial.c", "test/golden/factorial.out")
    , ("test/examples/splitmix.kmp", "test/harness/splitmix.c", "test/golden/splitmix.out")
    , ("lib/numbertheory.kmp", "test/harness/numbertheory.c", "test/golden/numbertheory.out")
    , ("test/examples/hamming.kmp", "test/harness/hamming.c", "test/golden/hamming.out")
    , ("test/examples/bool.kmp", "test/harness/bool.c", "test/golden/bool.out")
    , ("test/examples/const.kmp", "test/harness/const.c", "test/golden/const.out")
    , ("test/data/badCodegen.kmp", "test/harness/id.c", "test/golden/id.out")
    , ("test/data/mod.kmp", "test/harness/mod.c", "test/golden/mod.out")
    ]
