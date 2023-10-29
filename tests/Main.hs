module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.QuickCheck (testProperty)

import Parse.Term (terms)

main :: IO ()
main = defaultMain $ testGroup "Tests" [terms]

-- Test the identity `sin²θ ≡ (1 - cos 2θ) ÷ 2`
identity :: TestTree
identity = testProperty "Sin identity" \(θ :: Double) ->
  let l = sin θ * sin θ
      r = (1 - cos (2 * θ)) / 2
   in abs (l - r) < 0.0000001
