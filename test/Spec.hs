{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Backend
import           Harness
import           Parser
import           Test.Tasty
import           Type

main :: IO ()
main = defaultMain $
    testGroup "Kempe compiler tests"
        [ parserTests
        , typeTests
        , backendTests
        , testGroup "Golden output tests"
            [ goldenOutput "examples/factorial.kmp" "test/harness/factorial.c" "test/golden/factorial.out"
            , goldenOutput "test/examples/splitmix.kmp" "test/harness/splitmix.c" "test/golden/splitmix.out"
            , goldenOutput "lib/numbertheory.kmp" "test/harness/numbertheory.c" "test/golden/numbertheory.out"
            ]
        ]
