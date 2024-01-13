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
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad.Trans.Cofree (tailF)
import Data.Bifunctor (second)
import Data.Functor.Foldable (para)
import Data.Map (fromList, insert, singleton, union, (!))

import Infer.Value
import Syntax

eval :: Env -> ATm a -> Val
eval env = para (tailF >>> f)
 where
  f = \case
    -- Π and λ construct closures with their environment and body
    PiF x (_, v) (t, _) -> VPi x v (Clos env t)
    LamF x (t, _) -> VLam x (Clos env t)
    -- Application to a lambda reduces
    AppF (_, VLam x c) (_, v) -> apply c x v
    -- Application to anything else is neutral
    AppF (_, v₁) (_, v₂) -> VApp v₁ v₂
    -- Let is a local binding
    LetF bs (t, _) -> eval (binds bs `union` env) t
    -- Case tries to match the scrutinee against each pattern
    CaseF (_, v) ps -> matches v (second fst <$> ps)
    -- Symbols are looked up in the environment
    SymF n -> env ! n
    -- Constructors, unintuitively, self-evaluate
    -- This is because `Just 5` is represented as `VApp (VCon "Just") (VLit 5)`
    -- TODO: is this the right thing to do?
    ConF n -> VCon n
    -- Literals self-evaluate
    LitF n -> VLit n
    UF -> VU

  binds bs = fromList [(x, v) | (x, (_, v), _) <- bs]

  matches _ [] = error "Non-exhaustive patterns"
  matches v ((p, t) : ps) = case match p v of
    Nothing -> matches v ps
    Just e' -> eval (e' `union` env) t

-- | Evaluate a closusure with a symbol and value.
apply :: Clos -> Sym -> Val -> Val
apply (Clos env e) x v = eval (insert x v env) e

{- | Try to match a value against a pattern, producing the bound values.
Destructuring is recursive, so `F x y` matches `(F 5) 10`, producing `{x: 5, y: 10}`.
-}
match :: APat a -> Val -> Maybe Env
match = curry \case
  (_ :< (DestructF x ps), v) -> go x (reverse ps) v
  (_ :< (BindF x), v) -> Just (singleton x v)
  (_ :< (IsLitF n), VLit m) | n == m -> Just mempty
  (_ :< WildF, _) -> Just mempty
  _ -> Nothing
 where
  go x = curry \case
    ([], VCon y) | x == y -> Just mempty
    (p : ps, VApp v₁ v₂) -> liftA2 union (go x ps v₁) (match p v₂)
    _ -> Nothing
