{- | Term evaluation.
The implementation is a bit complex, but very elegant.
The term is evaluated using a paramorphism.
This means that every subterm is evaluated before the current term.
However, Π and λ terms need the original term to store in their closure, so a
paramorphism is used instead of a catamorphism to access both the current term
and the evaluated subterms.
This is safe due to purity - it's okay to evaluate the Π and λ bodies as they cannot
contain side effects. Additionally, due to Haskell's laziness, the subterms will
never be evaluated as they are not used.
-}
module Infer.Eval where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import Control.Comonad.Trans.Cofree qualified as CF
import Data.Bifunctor (bimap, first, second)
import Data.Functor.Foldable (para)
import Data.Map (fromList, insert, lookup, singleton, union)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (lookup)

import Infer.Value
import Syntax

{- | Evaluate a term under an environment, producing a value annotated
with annotations it originates from in the term.
-}
eval :: Env a -> Tm :@ a -> Val :@ a
eval env = para \(a CF.:< tm) -> case tm of
  -- Π and λ construct closures with their environment and body
  PiF x (_, v) (t, _) -> a :< VPiF x v (Clos env t)
  LamF x (t, _) -> a :< VLamF x (Clos env t)
  -- Application to a lambda reduces
  AppF (_, _ :< VLamF x c) (_, v) -> a :< unwrap (apply c x v)
  -- Application to anything else is neutral
  AppF (_, v₁) (_, v₂) -> a :< VAppF v₁ v₂
  -- Let is a local binding
  LetF bs (t, _) -> eval (binds bs `union` env) t
  -- Case tries to match the scrutinee against each pattern
  CaseF (_, v) ps -> matches v (second fst <$> ps)
  -- Symbols are looked up in the environment
  SymF n -> fromMaybe (a :< VSymF n) (lookup n env)
  -- Constructors, unintuitively, self-evaluate
  -- This is because `Just 5` is represented as `VApp (VCon "Just") (VLit 5)`
  -- TODO: is this the right thing to do?
  ConF n -> a :< VConF n
  -- Literals self-evaluate
  LitF n -> a :< VLitF n
  UF -> a :< VUF
 where
  binds =
    fromList . mapMaybe \case
      Def x _ (_, v) -> Just (x, v)
      _ -> Nothing

  matches _ [] = error "Non-exhaustive patterns"
  matches v ((p, t) : ps) = case match p v of
    Nothing -> matches v ps
    Just e' -> eval (e' `union` env) t

-- | Evaluate a closusure with a symbol and value.
apply :: Clos a -> Sym -> Val :@ a -> Val :@ a
apply (Clos env e) x v = eval (insert x v env) e

{- | Try to match a value against a pattern, producing the bound values.
Destructuring is recursive, so `F x y` matches `(F 5) 10`, producing `{x: 5, y: 10}`.
-}
match :: Pat :@ a -> Val :@ a -> Maybe (Env a)
match =
  curry $
    first unwrap >>> \case
      (DestructF x ps, v) -> go x (reverse ps) v
      (BindF x, v) -> Just (singleton x v)
      (IsLitF n, _ :< (VLitF m)) | n == m -> Just mempty
      (WildF, _) -> Just mempty
      _ -> Nothing
 where
  go x = curry \case
    ([], _ :< VConF y) | x == y -> Just mempty
    (p : ps, _ :< VAppF v₁ v₂) -> liftA2 union (go x ps v₁) (match p v₂)
    _ -> Nothing

-- | Beta-eta equality. Both values must have the same type!
conv :: Val :@ a -> Val :@ a -> Bool
conv =
  curry $
    bimap unwrap unwrap >>> \case
      (VPiF x τ c, VPiF y π d) ->
        conv τ π && conv (apply c x (sym x)) (apply d y (sym y))
      (VLamF x c, VLamF y d) ->
        conv (apply c x (sym x)) (apply d y (sym y))
      (VAppF e₁ e₂, VAppF e₃ e₄) ->
        conv e₁ e₃ && conv e₂ e₄
      (VSymF x, VSymF y) -> x == y
      (VConF x, VConF y) -> x == y
      (VLitF x, VLitF y) -> x == y
      (VUF, VUF) -> True
      _ -> False
 where
  sym x = error "oops!" :< VSymF x
