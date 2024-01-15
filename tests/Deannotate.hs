module Deannotate where

import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Functor.Foldable (cata)
import Syntax

-- TODO: figure out how to generalize this to a typeclass.
-- TODO: this is so verbose, there must be a better way.

deannotateTm :: ATm a -> Tm Pat
deannotateTm = cata f
 where
  f :: CofreeF (TmF (APat a)) a (Tm Pat) -> Tm Pat
  f (_ :< t) = case t of
    PiF x σ π -> Pi x σ π
    LamF x e -> Lam x e
    AppF e₁ e₂ -> App e₁ e₂
    LetF bs e -> Let bs e
    CaseF e ps -> Case e [(deannotatePat p, e') | (p, e') <- ps]
    SymF x -> Sym x
    ConF x -> Con x
    LitF n -> Lit n
    UF -> U

deannotatePat :: APat a -> Pat
deannotatePat = cata f
 where
  f :: CofreeF PatF a Pat -> Pat
  f (_ :< p) = case p of
    DestructF x ps -> Destruct x ps
    BindF x -> Bind x
    IsLitF n -> IsLit n
    WildF -> Wild
