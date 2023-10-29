module Parse.Term where

import Control.Monad (join)
import Data.Functor.Foldable (Base, Corecursive (embed))
import Data.Text qualified as T
import Parse (tm)
import Pretty ()
import Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Text.Megaparsec (eof, errorBundlePretty, parse)

-- | Unfortunately `recursion-schemes` does not provide this...
anaM :: (Corecursive b, Monad f, Traversable (Base b)) => (a -> f (Base b a)) -> a -> f b
anaM f x = embed <$> (f x >>= traverse (anaM f))

instance Arbitrary Tm where
  arbitrary = sized $ anaM \case
    0 -> oneof atoms
    n -> oneof (atoms <> [($ n `div` m) <$> f | (m, f) <- recs])
   where
    -- TODO: make `lower` and `upper` be arbitrary instances
    atoms =
      [ SymF <$> lower
      , ConF <$> upper
      , LitF <$> arbitrarySizedNatural
      , pure UF
      ]

    -- TODO: make functions be `[Int] -> Gen TmF`?
    recs :: [(Int, Gen (Int -> TmF Int))]
    recs =
      -- TODO: lambda and case
      [ (2, pure $ join (PiF "_"))
      , (2, join . PiF <$> lower)
          (2, pure $ join AppF)
      , (3, join . join . LetF <$> lower)
      ]

    lower = oneof (pure . T.singleton <$> "xyzαβγ")
    upper = oneof (pure . T.singleton <$> "XYZℕ")

instance Arbitrary Pat where
  arbitrary = sized $ anaM \case
    0 -> oneof atoms
    n -> oneof (atoms <> [DestructF <$> upper <*> pure (replicate 3 (n `div` 3))])
   where
    atoms = []
    upper = oneof (pure . T.singleton <$> "ABC")

terms :: TestTree
terms =
  testGroup
    "Terms"
    [testProperty "Random" \x -> x === p (T.pack $ show x)]
 where
  p x = case parse (tm <* eof) "<test>" x of
    Left e -> error (errorBundlePretty e)
    Right y -> y
