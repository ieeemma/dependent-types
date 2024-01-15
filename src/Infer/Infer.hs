module Infer.Infer where

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, asks, local)
import Data.Foldable (traverse_)
import Data.Map (fromList, insert, lookup, singleton)
import Data.Text (unpack)
import Prettyprinter (pretty)
import Prelude hiding (lookup)

import Infer.Eval (apply, conv, eval)
import Infer.Quote (quote)
import Infer.Value (Env, Val (..))
import Parse.Parse (Span)
import Parse.Pretty ()
import Syntax

data Ctx = Ctx {types :: Env, values :: Env}

bind :: Sym -> Val -> Infer a -> Infer a
bind x τ = local \c ->
  c{types = insert x τ (types c)}

binds :: Env -> Infer a -> Infer a
binds bs = local \c -> c{types = bs <> types c}

def :: Sym -> Val -> Val -> Infer a -> Infer a
def x τ v = local \c ->
  c{types = insert x τ (types c), values = insert x v (values c)}

type Infer = ReaderT Ctx (Except String)

same :: Val -> Val -> Infer ()
same τ π =
  unless (conv τ π) $
    throwError $
      "Expected " <> show (pretty (quote τ)) <> ", found " <> show (pretty (quote π))

-- | Evaluate a term using the environment within the monad.
evalM :: ATm Span -> Infer Val
evalM t = asks ((`eval` t) . values)

-- | Check that a term has a given type, then evaluate it.
ensure :: ATm Span -> Val -> Infer Val
ensure t τ = check t τ *> evalM t

-- | Bidirectional type checking.
check :: ATm Span -> Val -> Infer ()
check (sp :< tm) τ = case (tm, τ) of
  (LamF x e, VPi y σ c) -> bind x σ (check e $ apply c y (VSym x))
  (LetF bs e, _) -> let' bs (check e τ)
  _ -> do
    π <- infer (sp :< tm)
    same τ π

-- | Bidirectional type inference.
infer :: ATm Span -> Infer Val
infer (_ :< tm) = case tm of
  -- Π checks that σ is a type, then π is a type under x:σ
  PiF x σ π -> do
    σ' <- ensure σ VU
    bind x σ' (check π VU)
    pure VU
  -- λ cannot be inferred, only checked
  LamF _ _ -> throwError "Cannot infer lambda type, try annotating"
  -- Application checks that lhs is Π, then checks rhs against the domain
  AppF e₁ e₂ ->
    infer e₁ >>= \case
      VPi x τ c -> do
        check e₂ τ
        apply c x <$> evalM e₂
      _ -> throwError "Not a function"
  -- Let checks the bindings, then infers the body
  LetF bs e -> let' bs (infer e)
  CaseF e ps -> do
    σ <- infer e
    σs <- sequence [pat p σ >>= (`binds` infer v) | (p, v) <- ps]
    -- TODO: improve this error message, and fixup this code
    uncurry same `traverse_` zip σs (tail σs)
    pure (head σs)
  -- Symbols are looked up in the environment
  SymF x ->
    asks (lookup x . types) >>= \case
      Just v -> pure v
      Nothing -> throwError ("Unbound symbol " <> unpack x)
  ConF c ->
    asks (lookup c . types) >>= \case
      Just v -> pure v
      Nothing -> throwError ("Unbound symbol " <> unpack c)
  -- Literals are of type Int
  LitF _ -> pure (VCon "Int")
  -- U is a type
  UF -> pure VU

-- | Check the bindings of a let expression.
let' :: [Bind (ATm Span)] -> Infer a -> Infer a
let' [] m = m
let' (Def x σ e : bs) m = do
  σᵥ <- ensure σ VU
  eᵥ <- ensure e σᵥ
  def x σᵥ eᵥ (let' bs m)
let' (Data x σ cs : bs) m = do
  σᵥ <- ensure σ VU
  bind x σᵥ do
    let (xs, σs) = unzip cs
    σsᵥ <- (`ensure` VU) `traverse` σs
    let cs' = fromList (zip xs σsᵥ)
    binds cs' (let' bs m)

-- | Check that a pattern has a type, and return its binds.
pat :: APat Span -> Val -> Infer Env
pat = curry \case
  (_ :< (DestructF x ps), σ) -> go x (reverse ps) σ
  (_ :< (BindF x), σ) -> pure (singleton x σ)
  (_ :< (IsLitF _), VCon "Int") -> pure mempty
  (_ :< WildF, _) -> pure mempty
  (_, τ) -> throwError $ "Expected " <> show (pretty (quote τ))
 where
  go x = curry \case
    ([], VCon y) | x == y -> pure mempty
    (p : ps, VApp τ π) -> liftA2 (<>) (go x ps τ) (pat p π)
    (_, τ) -> throwError $ "Expected " <> show (pretty (quote τ))
