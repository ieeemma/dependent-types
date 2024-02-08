{- | Value quoting.
This reifies a value into a term.
Along with `eval`, this is used to implement normalization.
The implementation is very straightforward.
-}
module Infer.Quote where

import Control.Arrow ((>>>))
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree (tailF)
import Data.Functor.Foldable (cata)

import Infer.Eval (apply)
import Infer.Value
import Syntax

quote :: Val :@ a -> Tm Pat
quote =
  cata $ tailF >>> \case
    VPiF x e c -> Pi x e (clos c x)
    VLamF x c -> Lam x (clos c x)
    VAppF e₁ e₂ -> App e₁ e₂
    VSymF x -> Sym x
    VConF x -> Con x
    VLitF n -> Lit n
    VUF -> U
 where
  clos c x = quote (apply c x (error "TODO" :< VSymF x))
