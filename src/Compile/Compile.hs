module Compile.Compile where

import Control.Arrow ((>>>))
import Control.Comonad.Trans.Cofree (tailF, unwrap)
import Data.Bifunctor (second)
import Data.Functor.Foldable (para)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)

import Syntax (Bind (..), Pat, PatF (..), Tm, TmF (..), (:@))

-- | Compile a term to a Lisp expression.
compile :: Tm :@ a -> Text
compile = compileTerm >>> toLazyText >>> toStrict

compileTerm :: Tm :@ a -> Builder
compileTerm =
  para $
    tailF >>> \case
      PiF x _ (_, e) -> parens ["lambda", parens [fromText x], e]
      LamF x (_, e) -> parens ["lambda", parens [fromText x], e]
      AppF (_, e₁) (_, e₂) -> parens [e₁, e₂]
      LetF bs (_, e) -> parens ["letrec", parens (concatMap compileBind bs), e]
      CaseF (_, e) cs -> compileCase e (second snd <$> cs)
      SymF x -> fromText x
      ConF x -> fromText x
      LitF n -> compileLit n
      UF -> "Type"

compileBind :: Bind (Tm :@ a, Builder) -> [Builder]
compileBind = \case
  Def x _ (_, e) -> [parens [fromText x, e]]
  Data x _ cs -> let n = fromText x in parens [n, parens ["quote", n]] : fmap f cs
   where
    f (y, (σ, _)) = parens [fromText y, g y 0 σ]

    g y n =
      unwrap >>> \case
        PiF _ _ e -> parens ["lambda", parens [v n], g y (n + 1) e]
        _ -> parens $ ["list", parens ["quote", fromText y]] <> (v <$> [0 .. n - 1])

    v n = "$" <> decimal n

compileCase :: Builder -> [(Pat :@ a, Builder)] -> Builder
compileCase e cs = parens (h <> t)
 where
  h = ["match", e]
  t = [parens [compilePat p, e₂] | (p, e₂) <- cs]

compilePat :: Pat :@ a -> Builder
compilePat =
  unwrap >>> \case
    DestructF x ps ->
      parens $
        ["list", parens ["quote", fromText x]]
          <> fmap compilePat ps
    BindF x -> fromText x
    IsLitF n -> f n
    WildF -> "_"
 where
  f 0 = parens ["list", parens ["quote", "Z"]]
  f n = parens ["list", parens ["quote", "S"], f (n - 1)]

compileLit :: Int -> Builder
compileLit = \case
  0 -> "Z"
  n -> parens ["S", compileLit (n - 1)]

parens :: [Builder] -> Builder
parens xs = "(" <> mconcat (intersperse " " xs) <> ")"
