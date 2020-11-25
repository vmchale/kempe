module Main (main) where

import           Harness
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "Golden output tests"
        [ goldenOutput "examples/factorial.kmp" "test/harness/factorial.c" "test/golden/factorial.out"
        , goldenOutput "test/examples/splitmix.kmp" "test/harness/splitmix.c" "test/golden/splitmix.out"
        , goldenOutput "lib/numbertheory.kmp" "test/harness/numbertheory.c" "test/golden/numbertheory.out"
        ]
