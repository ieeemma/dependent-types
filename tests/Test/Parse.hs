{- | This module contains tests for the parser and pretty-printer.
It works by first generating random syntax trees (using `Arbitrary.hs` instances).
Then, it pretty-prints the syntax tree and parses it again.
The result should be the same as the original syntax tree.
This allows both to be automatically tested against infinite random inputs.
-}
module Test.Parse where

import Data.Text qualified as T
import Prettyprinter (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (===))
import Text.Megaparsec (eof, errorBundlePretty, parse)

import Arbitrary ()
import Deannotate (deannotatePat, deannotateTm)
import Parse.Parse (Parser, pat, term)
import Parse.Pretty ()

parseFrom :: Parser a -> String -> a
parseFrom p x = case parse (p <* eof) "<test>" (T.pack x) of
  Left e -> error (errorBundlePretty e)
  Right y -> y

parseTests :: TestTree
parseTests =
  testGroup
    "Parser"
    [ testProperty "Patterns" testPattern
    , testProperty "Terms" testTerm
    ]
 where
  testPattern p = deannotatePat (parseFrom pat (show (pretty p))) === p
  testTerm t = deannotateTm (parseFrom term (show (pretty t))) === t
