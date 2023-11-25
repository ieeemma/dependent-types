{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax where

import Control.Comonad.Cofree (Cofree)
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
data Tm p
  = Pi Sym (Tm p) (Tm p)
  | Lam Sym (Tm p)
  | App (Tm p) (Tm p)
  | Let [(Sym, Tm p, Tm p)] (Tm p)
  | Case (Tm p) [(p, Tm p)]
  | Sym Sym
  | Con Sym
  | Lit Int
  | U
  deriving (Eq)

makeBaseFunctor ''Tm

-- Aliases for annotations
type APat a = Cofree PatF a
type ATm a = Cofree (TmF (APat a)) a
