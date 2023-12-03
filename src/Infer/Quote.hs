{- | Value quoting.
This reifies a value into a term.
Along with `eval`, this is used to implement normalization.
The implementation is very straightforward.
-}
module Infer.Quote where

import Data.Functor.Foldable (cata)

import Infer.Eval (apply)
import Infer.Value
import Syntax

quote :: Val -> Tm Pat
quote = cata \case
  VPiF x e c -> Pi x e (clos c x)
  VLamF x c -> Lam x (clos c x)
  VAppF e₁ e₂ -> App e₁ e₂
  VSymF x -> Sym x
  VConF x -> Con x
  VLitF n -> Lit n
  VUF -> U
 where
  clos c x = quote (apply c x (VSym x))
