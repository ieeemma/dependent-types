{-# LANGUAGE ViewPatterns #-}

module Infer.Infer where

import Control.Applicative (liftA2)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad (unless)
import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (ReaderT, asks, local)
import Data.Foldable (traverse_)
import Data.Map (fromList, insert, lookup, singleton)
import Data.Text (Text)
import Error.Diagnose (Marker (This), Position (..), Report, err)

import Prelude hiding (lookup)

import Infer.Eval (apply, conv, eval)
import Infer.Quote (quote)
import Infer.Value (Clos (..), Env, Val (..))
import Parse.Parse (Span)
import Parse.Pretty (render)
import Syntax
import Util (fromSpan)

data Ctx = Ctx {types :: Env, values :: Env}

bind :: Sym -> Val -> Infer a -> Infer a
bind x τ = local \c ->
  c{types = insert x τ (types c)}

binds :: Env -> Infer a -> Infer a
binds bs = local \c -> c{types = bs <> types c}

def :: Sym -> Val -> Val -> Infer a -> Infer a
def x τ v = local \c ->
  c{types = insert x τ (types c), values = insert x v (values c)}

type Infer = ReaderT Ctx (Except (Report Text))

data Ty = Ty {_span :: Maybe Span, val :: Val}

-- type Ty = Cofree ValF (Maybe Span)

u :: Ty
u = Ty Nothing VU

-- | Produce a marker for a type and message.
marker :: Ty -> (Text -> Text) -> (Position, Marker Text)
marker (Ty sp τ) f =
  (maybe (Position (0, 0) (0, 0) "") fromSpan sp, This (f $ render (quote τ)))

-- | Produce a type error from a message and a list of types and messages.
typeError :: Text -> [(Position, Marker Text)] -> Infer a
typeError msg ms = throwError (err Nothing msg ms [])

-- | Assert that two types are equal, or produce a type error.
same :: Ty -> Ty -> Infer ()
same τ π = unless (conv (val τ) (val π)) $ typeError "Type mismatch" ms
 where
  ms = [marker τ ("Expected " <>), marker π ("Got " <>)]

-- | Evaluate a term using the environment within the monad.
evalM :: ATm Span -> Infer Val
evalM t = asks ((`eval` t) . values)

apply' :: Clos -> Sym -> Val -> Ty
apply' c@(Clos _ (sp :< _)) x v =
  Ty (Just sp) (apply c x v)

-- | Check that a term has a given type, then evaluate it.
ensure :: ATm Span -> Ty -> Infer Ty
ensure t@(sp :< _) τ = do
  check t τ
  Ty (Just sp) <$> evalM t

-- | Bidirectional type checking.
check :: ATm Span -> Ty -> Infer ()
check (sp :< tm) τ = case (tm, τ) of
  -- TODO: make clos store span, then fix this.
  (LamF x e, val -> VPi y σ c) ->
    bind x σ (check e $ apply' c y (VSym x))
  (LetF bs e, _) -> let' bs (check e τ)
  _ -> do
    π <- infer (sp :< tm)
    same τ π

-- | Bidirectional type inference.
infer :: ATm Span -> Infer Ty
infer (sp :< tm) = case tm of
  -- Π checks that σ is a type, then π is a type under x:σ
  PiF x σ π -> do
    σ' <- ensure σ u
    bind x (val σ') (check π u)
    pure (ty VU)
  -- λ cannot be inferred, only checked
  LamF _ _ -> typeError "Cannot infer type of lambda" [(fromSpan sp, This "")]
  -- Application checks that lhs is Π, then checks rhs against the domain
  AppF e₁ e₂ ->
    infer e₁ >>= \case
      (Ty sp' (VPi x τ c)) -> do
        -- TODO: Unfortunately, a span has to be invented here.
        -- It should correspond to the lhs of e₁.
        check e₂ (Ty sp' τ)
        apply' c x <$> evalM e₂
      τ -> typeError "Cannot call non-function" [marker τ ("Has type " <>)]
  -- Let checks the bindings, then infers the body
  LetF bs e -> let' bs (infer e)
  CaseF e ps -> do
    σ <- infer e
    vs <- sequence [pat p σ >>= (`binds` infer v) | (p, v) <- ps]
    -- TODO: improve this error message, and fixup this code
    uncurry same `traverse_` zip vs (tail vs)
    pure (head vs)
  -- Symbols are looked up in the environment
  SymF x ->
    asks (lookup x . types) >>= \case
      Just v -> pure (ty v)
      -- TODO: add 'check' case when type is known
      Nothing -> typeError "Unbound symbol" [(fromSpan sp, This "")]
  ConF c ->
    asks (lookup c . types) >>= \case
      Just v -> pure (ty v)
      Nothing -> typeError "Unbound constructor" [(fromSpan sp, This "")]
  -- Literals are of type Int
  LitF _ -> pure (ty $ VCon "Int")
  -- U is a type
  UF -> pure (ty VU)
 where
  ty = Ty (Just sp)

-- | Check the bindings of a let expression.
let' :: [Bind (ATm Span)] -> Infer a -> Infer a
-- let' = undefined
let' [] m = m
let' (Def x σ e : bs) m = do
  σᵥ <- ensure σ u
  eᵥ <- ensure e σᵥ
  def x (val σᵥ) (val eᵥ) (let' bs m)
let' (Data x σ cs : bs) m = do
  σᵥ <- ensure σ u
  bind x (val σᵥ) do
    let (xs, σs) = unzip cs
    σsᵥ <- (`ensure` u) `traverse` σs
    let cs' = fromList (zip xs (val <$> σsᵥ))
    binds cs' (let' bs m)

-- | Check that a pattern has a type, and return its binds.
pat :: APat Span -> Ty -> Infer Env
pat = undefined

-- pat = curry \case
--   (_ :< (DestructF x ps), σ) -> go x (reverse ps) σ
--   (_ :< (BindF x), σ) -> pure (singleton x σ)
--   (_ :< (IsLitF _), VCon "Int") -> pure mempty
--   (_ :< WildF, _) -> pure mempty
--   -- TODO: add inferred type to error
--   (sp :< _, τ) -> typeError ("Expected " <> render (quote τ)) sp ""
--  where
--   -- TODO: does this... work? I think it needs to examine in-scope
--   -- constructors instead.
--   go x = curry \case
--     ([], VCon y) | x == y -> pure mempty
--     (p : ps, VApp τ π) -> liftA2 (<>) (go x ps τ) (pat p π)
--     -- TODO: figure out the error cases here
--     _ -> error "Unimplemented"
