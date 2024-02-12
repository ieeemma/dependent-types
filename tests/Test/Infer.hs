{-# LANGUAGE PatternSynonyms #-}

module Test.Infer where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Deannotate (deannotateTm)
import Infer.Infer (Ctx (..), infer)
import Infer.Quote (quote)
import Parse.Parse (term)
import Syntax
import Test.Parse (parseFrom)

inferTests :: TestTree
inferTests =
  testGroup
    "Infer"
    [testCase n (typeof τ @?= deannotateTm (parseFrom term v)) | (n, τ, v) <- cases]

typeof :: String -> Tm Pat
typeof x =
  let τ = infer (parseFrom term x)
      r = runExcept (runReaderT τ (Ctx mempty mempty))
   in case r of
        Right x -> quote x
        -- TODO: Until the `diagnose` library unpins its `text` dependency,
        -- the `Report` type is inaccessible, so can't be tested
        Left _ -> error "oops"

cases :: [(String, String, String)]
cases =
  [ -- Literals
    ("lit", "3", "Int")
  , ("u", "Type", "Type")
  , -- Π
    ("Π", "(x: Type) -> Maybe x", "Type")
  , -- app
    ("app", "Just 5", "Maybe Int")
  ]
