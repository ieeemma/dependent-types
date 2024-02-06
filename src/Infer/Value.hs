{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This module implements the values of the language.
Values mostly follow the same structure as terms.
Type checking converts terms to values (`eval`) and values to terms (`quote`).
-}
module Infer.Value where

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map (Map)

import Syntax

-- TODO: De-Bruijn indices for better performance.

{- | Values mirror terms, but without `let` and `case`.
Also stores closures on Π and λ.
-}
data Val c
  = VPi Sym (Val c) c
  | VLam Sym c
  | VApp (Val c) (Val c)
  | VSym Sym
  | VCon Sym
  | VLit Int
  | VU

{- | Closures store the environment and the body of a function.
This captures the scope of a function.
For example, in `λx -> fx`, the closure captures the value of `f` as
it is free.
-}
data Clos a = Clos (Env a) (Tm :@ a)

-- | Environments map symbols to values.
type Env a = Map Sym (Val :@ a)

makeBaseFunctor ''Val

type instance Val :@ a = Cofree (ValF (Clos a)) a
