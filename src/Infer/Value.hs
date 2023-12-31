{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This module implements the values of the language.
Values mostly follow the same structure as terms.
Type checking converts terms to values (`eval`) and values to terms (`quote`).
-}
module Infer.Value where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.HashMap.Strict (HashMap)

import Syntax

-- TODO: De-Brauijn indices for better performance.

{- | Values mirror terms, but without `let` and `case`.
Also stores closures on Π and λ.
-}
data Val
  = VPi Sym Val Clos
  | VLam Sym Clos
  | VApp Val Val
  | VSym Sym
  | VCon Sym
  | VLit Int
  | VU

{- | Closures store the environment and the body of a function.
This captures the scope of a function.
For example, in `λx -> fx`, the closure captures the value of `f` as
it is free.
-}
data Clos = Clos Env (Tm Pat)

{- | Environments map symbols to values.
A `HashMap` is used over a `Map` here as they are generally faster,
but have worse union performance.
This might need to be changed in the future.
-}
type Env = HashMap Sym Val

makeBaseFunctor ''Val
