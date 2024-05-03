module Test.Compile where

import Data.Text (Text, unpack, unlines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import System.Process (readProcess)
import Prelude hiding (unlines)

import Compile.Compile (compile)
import Parse.Parse (Span, file, mainTermWith, term)
import Syntax (Bind, Tm, (:@))
import Test.Eval (cases)
import Test.Parse (parseFrom')

compileTests :: TestTree
compileTests =
  testGroup
    "Compile"
    [testCase n (sameCompile x y) | (n, x, y) <- cases]

header :: Text
header =
  unlines
    [ "data Bool : Type { True : Bool; False: Bool; };"
    , "data ℕ : Type { Z : ℕ; S : ℕ -> ℕ; };"
    , "data Maybe : Type -> Type { Some : (a: Type) -> a -> Maybe a; None : (a: Type) -> Maybe a; };"
    ]

std :: [Bind (Tm :@ Span)]
std = parseFrom' file header

sameCompile :: Text -> Text -> Assertion
sameCompile x y = do
  let x' = compile (mainTermWith std (parseFrom' term x))
      y' = compile (mainTermWith std (parseFrom' term y))
   in do
      vx <- readProcess "racket" ["-e", unpack x'] ""
      vy <- readProcess "racket" ["-e", unpack y'] ""
      vx @?= vy
