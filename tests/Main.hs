module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Eval (evalTests)
import Test.Parse (parseTests)

main :: IO ()
main = defaultMain $ testGroup "Tests" [parseTests, evalTests]
