module Test.Eval where

import Infer.Eval (eval)
import Infer.Quote (quote)
import Test.Parse (parseFrom)

import Deannotate (deannotateTm)
import Parse.Parse (term)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))

evalTests :: TestTree
evalTests =
  testGroup
    "Eval"
    [testCase n (x `evalsTo` v) | (n, x, v) <- cases]

evalsTo :: String -> String -> Assertion
evalsTo x v =
  let x' = parseFrom term x
      v' = parseFrom term v
   in quote (eval mempty x') @=? deannotateTm v'

cases :: [(String, String, String)]
cases =
  [ -- Self-eval tests
    ("lit", "3", "3")
  , ("con", "True", "True")
  , ("app", "Just 5", "Just 5")
  , ("λ", "λx -> x", "λx -> x")
  , ("Π", "(x: Type) -> Maybe x", "(x: Type) -> Maybe x")
  , -- β reduction rule
    ("id 5", "(λx -> x) 5", "5")
  , ("const 5 6", "(λx -> λy -> x) 5 6", "5")
  , ("partial const", "(λx -> λy -> x) 5", "λy -> 5")
  , -- Let tests
    ("single let", "let { x : ℕ = 5; } in x", "5")
  , ("multi let", "let { x : ℕ = 5; y : ℕ = 10; } in y", "10")
  , ("recursive", "let { x : ℕ = 5; y : ℕ = x } in x", "5")
  ]
