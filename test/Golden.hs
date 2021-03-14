module Main (main) where

import           Harness
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "Golden output tests"
        [ goldenOutput "examples/factorial.kmp" "test/harness/factorial.c" "test/golden/factorial.out"
        , goldenOutput "test/examples/splitmix.kmp" "test/harness/splitmix.c" "test/golden/splitmix.out"
        , goldenOutput "lib/numbertheory.kmp" "test/harness/numbertheory.c" "test/golden/numbertheory.out"
        , goldenOutput "test/examples/hamming.kmp" "test/harness/hamming.c" "test/golden/hamming.out"
        , goldenOutput "test/examples/bool.kmp" "test/harness/bool.c" "test/golden/bool.out"
        -- put them here since they take long
        , compileArm "examples/factorial.kmp"
        , compileArm "lib/numbertheory.kmp"
        -- , compileArm "lib/gaussian.kmp"
        ]
