module Main (main) where

import           Harness
import           System.Info (arch)
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "Golden output tests" $
        [ goldenOutput "examples/factorial.kmp" "test/harness/factorial.c" "test/golden/factorial.out"
        , goldenOutput "test/examples/splitmix.kmp" "test/harness/splitmix.c" "test/golden/splitmix.out"
        , goldenOutput "lib/numbertheory.kmp" "test/harness/numbertheory.c" "test/golden/numbertheory.out"
        , goldenOutput "test/examples/hamming.kmp" "test/harness/hamming.c" "test/golden/hamming.out"
        , goldenOutput "test/examples/bool.kmp" "test/harness/bool.c" "test/golden/bool.out"
        , goldenOutput "test/examples/const.kmp" "test/harness/const.c" "test/golden/const.out"
        ] ++ crossTests

-- These are redundant on arm
crossTests :: [TestTree]
crossTests = case arch of
    "x86_64" -> [ compileArm "examples/factorial.kmp"
                , compileArm "lib/numbertheory.kmp"
                , compileArm "test/examples/bool.kmp"
                , compileArm "examples/splitmix.kmp"
                , compileArm "lib/gaussian.kmp"
                ]
    "aarch64" -> []
    _ -> error "Test suite must be run on x86_64 or aarch64"
