{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)

type Sym = Text

{- | Patterns can be one of:
  * C p₁ … pₙ
  * x
  * n
  * _
-}
data Pat
  = Destruct Sym [Pat]
  | Bind Sym
  | IsLit Int
  | Wild
  deriving (Eq)

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
  = Pi Sym Tm Tm
  | Lam Sym Tm
  | App Tm Tm
  | Let [(Sym, Tm, Tm)] Tm
  | Case Tm [(Pat, Tm)]
  | Sym Sym
  | Con Sym
  | Lit Int
  | U
  deriving (Eq)

makeBaseFunctor ''Tm
