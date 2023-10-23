{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)

type Name = Text

{- | Patterns can be one of:
  * C p₁ … pₙ
  * x
  * n
  * _
-}
data Pat
  = Destruct Text [Pat]
  | Bind Text
  | IsLit Int
  | Wild

makeBaseFunctor ''Pat

{- | Terms can be one of:
  * Π x σ π
  * λ x e
  * e₁ e₂
  * let x: σ = e₁ in e₂
  * case e { p₁ -> e₁ … pₙ -> eₙ }
  * x
  * n
  * σ
-}
data Tm
  = Pi Name Tm Tm
  | Lam Name Tm
  | App Tm Tm
  | Let Name Tm Tm Tm
  | Case Tm [(Pat, Tm)]
  | Sym Name
  | Lit Int
  | U

makeBaseFunctor ''Tm
