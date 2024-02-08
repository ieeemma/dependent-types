{-# LANGUAGE ViewPatterns #-}

module Infer.Infer where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree ((:<)), unwrap)
import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, asks, local)
import Data.Foldable (traverse_)
import Data.Map (fromList, insert, lookup, singleton)
import Data.Text (Text)
import Error.Diagnose (Marker (This), Position (..), Report, err)
import Text.Megaparsec (SourcePos (..), mkPos)
import Prelude hiding (lookup)

import Infer.Eval (apply, conv, eval)
import Infer.Quote (quote)
import Infer.Value (Env, Val (..), ValF (..))
import Parse.Parse (Span (..))
import Parse.Pretty (render)
import Syntax
import Util (fromSpan)

nospan :: f (Cofree f Span) -> Cofree f Span
nospan = (Span x x :<) where x = SourcePos "" (mkPos 0) (mkPos 0)

u :: Val :@ Span
u = nospan VUF

data Ctx = Ctx {types :: Env Span, values :: Env Span}

bind :: Sym -> Val :@ Span -> Infer a -> Infer a
bind x τ = local \c ->
  c{types = insert x τ (types c)}

binds :: Env Span -> Infer a -> Infer a
binds bs = local \c -> c{types = bs <> types c}

def :: Sym -> Val :@ Span -> Val :@ Span -> Infer a -> Infer a
def x τ v = local \c ->
  c{types = insert x τ (types c), values = insert x v (values c)}

type Infer = ReaderT Ctx (Except (Report Text))

-- | Produce a marker for a type and message.
marker :: Val :@ Span -> (Text -> Text) -> (Position, Marker Text)
marker τ f = (fromSpan (extract τ), This (f $ render (quote τ)))

-- | Produce a type error from a message and a list of types and messages.
typeError :: Text -> [(Position, Marker Text)] -> Infer a
typeError msg ms = throwError (err Nothing msg ms [])

-- | Assert that two types are equal, or produce a type error.
same :: Val :@ Span -> Val :@ Span -> Infer ()
same τ π = unless (conv τ π) $ typeError "Type mismatch" ms
 where
  ms = [marker π ("Got " <>), marker τ ("Expected " <>)]

-- | Evaluate a term using the environment within the monad.
evalM :: Tm :@ Span -> Infer (Val :@ Span)
evalM t = asks (values >>> (`eval` t))

-- | Check that a term has a given type, then evaluate it.
ensure :: Tm :@ Span -> Val :@ Span -> Infer (Val :@ Span)
ensure t τ = check t τ *> evalM t

-- | Bidirectional type checking.
check :: Tm :@ Span -> Val :@ Span -> Infer ()
check = curry \case
  (_ :< (LamF x e), _ :< (VPiF y τ c)) ->
    bind x τ (check e (apply c y (nospan (VSymF x))))
  (e, τ) -> infer e >>= same τ

-- | Bidirectional type inference.
infer :: Tm :@ Span -> Infer (Val :@ Span)
infer (sp :< tm) = case tm of
  -- Π checks that σ is a type, then π is a type under x:σ
  PiF x σ π -> do
    σ' <- ensure σ u
    bind x σ' (check π u)
    pure (sp :< VUF)
  LamF _ _ -> typeError "Cannot infer type of lambda" [(fromSpan sp, This "")]
  AppF e₁ e₂ ->
    infer e₁ >>= \case
      (unwrap -> VPiF x τ c) -> do
        check e₂ τ
        (apply c x >>> respan) <$> evalM e₂
      τ -> typeError "Cannot call non-function" [marker τ ("Has type " <>)]
  LetF bs e -> let' bs (infer e)
  CaseF e ps -> do
    σ <- infer e
    vs <- sequence [pat p σ >>= (`binds` infer v) | (p, v) <- ps]
    -- TODO: improve this error message, and fixup this code
    uncurry same `traverse_` zip vs (tail vs)
    pure (head vs)
  -- Symbols and constructors are looked up in the environment
  -- Also replaces the existing span annotation
  SymF x ->
    asks (types >>> lookup x) >>= \case
      Just v -> pure (respan v)
      -- TODO: add 'check' case when type is known
      Nothing -> typeError "Unbound symbol" [(fromSpan sp, This "")]
  ConF x ->
    asks (types >>> lookup x) >>= \case
      Just v -> pure (respan v)
      -- TODO: add 'check' case when type is known
      Nothing -> typeError "Unbound constructor" [(fromSpan sp, This "")]
  -- Literals are of type Int
  LitF _ -> pure (sp :< VConF "Int")
  -- U is a type
  UF -> pure (sp :< VUF)
 where
  respan x = sp :< unwrap x

-- | Check the bindings of a let expression.
let' :: [Bind (Tm :@ Span)] -> Infer a -> Infer a
let' [] m = m
let' (Def x σ e : bs) m = do
  σᵥ <- ensure σ u
  eᵥ <- ensure e σᵥ
  def x σᵥ eᵥ (let' bs m)
let' (Data x σ cs : bs) m = do
  σᵥ <- ensure σ u
  bind x σᵥ do
    let (xs, σs) = unzip cs
    σsᵥ <- (`ensure` u) `traverse` σs
    let cs' = fromList (zip xs σsᵥ)
    binds cs' (let' bs m)

-- TODO: refactor this to avoid `:<`
pat :: Pat :@ Span -> Val :@ Span -> Infer (Env Span)
pat = curry $ \case
  (sp :< DestructF x ps, τ) ->
    go x (reverse ps) τ >>= \case
      Just e -> pure e
      Nothing -> typeError "Type mismatch" [marker (sp :< unwrap τ) ("Expected " <>)]
  (_ :< BindF x, τ) -> pure (singleton x τ)
  (_ :< IsLitF _, _ :< VConF "Int") -> pure mempty
  (_ :< WildF, _) -> pure mempty
  (sp :< _, _ :< τ) -> typeError "Type mismatch" [marker (sp :< τ) ("Expected " <>)]
 where
  go x = curry \case
    ([], _ :< VConF y) | x == y -> pure (Just mempty)
    (p : ps, _ :< VAppF τ π) -> liftA2 (<>) (go x ps τ) (Just <$> pat p π)
    _ -> pure Nothing
