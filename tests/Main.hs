module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Eval (evalTests)
import Test.Infer (inferTests)
import Test.Parse (parseTests)

main :: IO ()
main = do
  i <- inferTests
  defaultMain $ testGroup "Tests" [parseTests, evalTests, i]
