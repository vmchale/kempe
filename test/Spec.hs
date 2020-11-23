{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Backend
import           Parser
import           Test.Tasty
import           Type

main :: IO ()
main = defaultMain $
    testGroup "Kempe compiler tests"
        [ parserTests
        , typeTests
        , backendTests
        ]
