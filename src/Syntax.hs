{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Syntax where

import Control.Comonad.Cofree (Cofree)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Text (Text)
import GHC.Generics (Generic)

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
  deriving (Eq, Show)

makeBaseFunctor ''Pat

{- | Bindings can be one of:
  * def x: σ = e
  * data C: σ where (C₁: σ₁) | … | (Cₙ: σₙ)
-}
data Bind t
  = Def Sym t t
  | Data Sym t [(Sym, t)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

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
  | Let [Bind (Tm p)] (Tm p)
  | Case (Tm p) [(p, Tm p)]
  | Sym Sym
  | Con Sym
  | Lit Int
  | U
  deriving (Eq, Show)

makeBaseFunctor ''Tm

-- Type family for 'passing through' an annotation.

type family (a :: k) :@ b

type instance Pat :@ a = Cofree PatF a
type instance Tm :@ a = Cofree (TmF (Pat :@ a)) a

deriving instance Generic Pat
deriving instance Generic (Tm a)

-- type APat a = Cofree PatF a
-- type ATm a = Cofree (TmF (APat a)) a
