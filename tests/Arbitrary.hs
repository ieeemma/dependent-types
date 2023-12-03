{-# LANGUAGE DataKinds #-}

{- | This module implements generation of random syntax trees.
This is used for random test generation.
We're using the `generic-random` package to generate random values for each
constructor using a uniform distribution.

Custom generators are inserted to produce uppercase constructor names.
-}
module Arbitrary () where

import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Generic.Random (constrGen, genericArbitraryG, genericArbitraryRecG, uniform, withBaseCase)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, oneof)

import Syntax

-- TODO: use newtypes for lower/upper rather than custom generators.

lower, upper :: Gen T.Text
lower = oneof (pure . T.singleton <$> "xyzαβ")
upper = oneof (pure . T.singleton <$> "XYZℕΣ")

instance Arbitrary Sym where
  arbitrary = lower

instance (Arbitrary t) => Arbitrary (Tl t) where
  arbitrary = genericArbitraryG gens uniform
   where
    gens = constrGen (Proxy :: Proxy '("Data", 0)) upper

instance (Arbitrary p) => Arbitrary (Tm p) where
  arbitrary = genericArbitraryRecG gens uniform `withBaseCase` base
   where
    gens = constrGen (Proxy :: Proxy '("Con", 0)) upper
    base =
      oneof
        [ Sym <$> arbitrary
        , Con <$> upper
        , Lit <$> arbitrary
        , pure U
        ]

instance Arbitrary Pat where
  arbitrary = genericArbitraryRecG gens uniform `withBaseCase` base
   where
    gens = constrGen (Proxy :: Proxy '("Destruct", 0)) upper
    base =
      oneof
        [ Bind <$> arbitrary
        , IsLit <$> arbitrary
        , pure Wild
        ]
