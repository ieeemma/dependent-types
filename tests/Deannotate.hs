module Deannotate where

import Control.Arrow ((>>>))
import Control.Comonad.Trans.Cofree (tailF)
import Data.Functor.Foldable (Corecursive (embed), cata)
import Infer.Value
import Syntax

-- TODO: figure out how to generalize this to a typeclass.
-- TODO: this is so verbose, there must be a better way.

deannotateTm :: Tm :@ a -> Tm Pat
deannotateTm =
  cata $ tailF >>> \case
    PiF x σ π -> Pi x σ π
    LamF x e -> Lam x e
    AppF e₁ e₂ -> App e₁ e₂
    LetF bs e -> Let bs e
    CaseF e ps -> Case e [(deannotatePat p, e') | (p, e') <- ps]
    SymF x -> Sym x
    ConF x -> Con x
    LitF n -> Lit n
    UF -> U

deannotatePat :: Pat :@ a -> Pat
deannotatePat = cata $ tailF >>> embed

deannotateVal :: Val :@ a -> Val (Clos a)
deannotateVal = cata $ tailF >>> embed
