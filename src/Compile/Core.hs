{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- | This module provides a simplified syntax datastructure used
for compilation.
-}
module Compile.Core where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import Syntax (Sym)

{- | Core terms can be one of:
  * λ x e
  * e₁ e₂
  * x
  * n
-}
data Core
    = Lam Sym Core
    | App Core Core
    | Sym Sym
    | Lit Int
    deriving (Eq, Show)

makeBaseFunctor ''Core
