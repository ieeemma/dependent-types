module Infer.Unify where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Except (Except)
import Data.Map (Map, lookup, singleton)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Error.Diagnose (Report)
import Prelude hiding (lookup)

import Infer.Error
import Infer.Eval (conv)
import Infer.Value (Clos (..), Env, Val (..), ValF (..))
import Parse.Parse (Span (..))
import Syntax

unify :: Val :@ Span -> Val :@ Span -> Except (Report Text) (Env Span)
unify = curry \case
  (_ :< VAppF τ₁ τ₂, _ :< VAppF π₁ π₂) -> do
    s₁ <- unify τ₁ π₁
    s₂ <- unify (subst s₁ τ₂) (subst s₁ π₂)
    pure (merge s₁ s₂)
  (_ :< VSymF x, τ) -> pure (singleton x τ)
  (τ, sp :< VSymF x) -> unify (sp :< VSymF x) τ
  (τ, π) | conv τ π -> pure mempty
  (τ, π) ->
    typeError "Could not unify types" [marker τ id, marker π id]

merge :: Env Span -> Env Span -> Env Span
merge s₁ s₂ = subst s₂ s₁ <> s₂

class Subst a where
  subst :: Env Span -> a -> a

instance Subst (Cofree (ValF (Clos Span)) Span) where
  subst s (sp :< v) = case v of
    VPiF x τ c -> sp :< VPiF x (subst s τ) (subst s c)
    VLamF x c -> sp :< VLamF x (subst s c)
    VAppF τ π -> sp :< VAppF (subst s τ) (subst s π)
    VSymF x -> fromMaybe (sp :< VSymF x) (lookup x s)
    VConF x -> sp :< VConF x
    VLitF n -> sp :< VLitF n
    VUF -> sp :< VUF

instance Subst (Clos Span) where
  subst s (Clos e c) = Clos (subst s e) c

instance (Subst v) => Subst (Map k v) where
  subst s = fmap (subst s)
