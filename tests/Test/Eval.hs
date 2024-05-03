module Test.Eval where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Data.Text (Text)
import Deannotate (deannotateTm)
import Infer.Eval (eval)
import Infer.Quote (quote)
import Parse.Parse (term)
import Test.Parse (parseFrom')

evalTests :: TestTree
evalTests =
  testGroup
    "Eval"
    [testCase n (x `evalsTo` v) | (n, x, v) <- cases]

evalsTo :: Text -> Text -> Assertion
evalsTo x v =
  let x' = parseFrom' term x
      v' = parseFrom' term v
   in quote (eval mempty x') @?= deannotateTm v'

cases :: [(String, Text, Text)]
cases =
  [ -- Self-eval tests
    ("lit", "3", "3")
  , ("con", "True", "True")
  , ("app", "Some 5", "Some 5")
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
