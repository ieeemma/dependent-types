module Infer.Infer where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, asks, local)
import Data.Map (insert, lookup)
import Prettyprinter (pretty)
import Prelude hiding (lookup)

import Infer.Eval (apply, eval)
import Infer.Quote (quote)
import Infer.Value (Env, Val (..))
import Parse.Parse (Span)
import Parse.Pretty ()
import Syntax

data Ctx = Ctx {types :: Env, values :: Env}

-- It doesn't seem worth importing a lens library just for this...
-- bindₜ, bindᵥ :: Sym -> Val -> Infer a -> Infer a
-- bindₜ x τ = local (\c -> c{types = insert x τ (types c)})
-- bindᵥ x v = local (\c -> c{values = insert x v (values c)})

bind :: Sym -> Val -> Infer a -> Infer a
bind x τ = local \c ->
  c{types = insert x τ (types c)}

def :: Sym -> Val -> Val -> Infer a -> Infer a
def x τ v = local \c ->
  c{types = insert x τ (types c), values = insert x v (values c)}

-- TODO:
-- [x] pass around a context of variable types and current env
-- [ ] implement `binds` using this and update `eval`
-- [ ] pass around constructors in the context
-- [ ] update constr inference and pattern matching to use this
-- [ ] implement pattern matching inference
type Infer = ReaderT Ctx (Except String)

-- | Bidirectional type checking.
check :: ATm Span -> Val -> Infer ()
check (sp :< tm) τ = case (tm, τ) of
  (LamF x e, VPi y σ c) -> bind x σ (check e $ apply c y (VSym x))
  (LetF bs e, _) -> binds bs (check e τ)
  _ -> do
    π <- infer (sp :< tm)
    unless (conv π τ) $
      throwError $
        "Expected " <> show (pretty (quote τ)) <> ", found " <> show (pretty (quote π))

-- | Bidirectional type inference.
infer :: ATm Span -> Infer Val
infer (_ :< tm) = case tm of
  -- Π checks that σ is a type, then π is a type under x:σ
  PiF x σ π -> do
    check σ VU
    σ' <- asks ((`eval` σ) . values)
    bind x σ' (check π VU)
    pure VU
  -- λ cannot be inferred, only checked
  LamF _ _ -> throwError "Cannot infer lambda type, try annotating"
  -- Application checks that lhs is Π, then checks rhs against the domain
  AppF e₁ e₂ ->
    infer e₁ >>= \case
      VPi x τ c -> do
        check e₂ τ
        asks $ apply c x . flip eval e₂ . values
      _ -> throwError "Not a function"
  -- Let checks the bindings, then infers the body
  LetF bs e -> binds bs (infer e)
  CaseF _ _ -> throwError "Not implemented"
  -- Symbols are looked up in the environment
  SymF x ->
    asks (lookup x . types) >>= \case
      Just v -> pure v
      Nothing -> throwError "Unbound symbol"
  ConF c ->
    asks (lookup c . types) >>= \case
      Just v -> pure v
      Nothing -> throwError "Unbound symbol"
  -- Literals are of type Int
  LitF _ -> pure (VCon "Int")
  -- U is a type
  UF -> pure VU

-- | Beta-eta equality. Both values must have the same type!
conv :: Val -> Val -> Bool
conv = curry \case
  (VPi x τ c, VPi y σ d) ->
    conv τ σ && conv (apply c x (VSym x)) (apply d y (VSym y))
  (VLam x c, VLam y d) ->
    conv (apply c x (VSym x)) (apply d y (VSym y))
  (VApp e₁ e₂, VApp e₃ e₄) ->
    conv e₁ e₃ && conv e₂ e₄
  (VSym x, VSym y) -> x == y
  (VCon x, VCon y) -> x == y
  (VLit x, VLit y) -> x == y
  (VU, VU) -> True
  _ -> False

-- | Check the bindings of a let expression.
binds :: [Bind (ATm Span)] -> Infer a -> Infer a
binds bs m = error "TODO"
