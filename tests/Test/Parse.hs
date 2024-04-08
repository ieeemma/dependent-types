{- | This module contains tests for the parser and pretty-printer.
It works by first generating random syntax trees (using `Arbitrary.hs` instances).
Then, it pretty-prints the syntax tree and parses it again.
The result should be the same as the original syntax tree.
This allows both to be automatically tested against infinite random inputs.
-}
module Test.Parse where

import Control.Arrow ((>>>))
import Data.Text (Text)
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (===))
import Text.Megaparsec (eof, errorBundlePretty, parse)

import Arbitrary ()
import Deannotate (deannotatePat, deannotateTm)
import Parse.Parse (Parser, pat, term)
import Parse.Pretty ()

parseFrom :: Parser a -> FilePath -> Text -> a
parseFrom p path x = case parse (p <* eof) path x of
  Left e -> error (errorBundlePretty e)
  Right y -> y

parseFrom' :: Parser a -> Text -> a
parseFrom' = flip parseFrom "<test>"

parseTests :: TestTree
parseTests =
  testGroup
    "Parser"
    [ testProperty "Patterns" testPattern
    , testProperty "Terms" testTerm
    ]
 where
  testPattern p = deannotatePat (parseFrom' pat (prettyText p)) === p
  testTerm t = deannotateTm (parseFrom' term (prettyText t)) === t

prettyText :: (Pretty a) => a -> Text
prettyText = pretty >>> layoutPretty defaultLayoutOptions >>> renderStrict
