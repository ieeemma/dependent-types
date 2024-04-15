module Compile.Compile where

import Control.Arrow ((>>>))
import Control.Comonad.Trans.Cofree (tailF, unwrap)
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata)
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
  cata $ tailF >>> \case
    LamF x e -> parens ["lambda", parens [fromText x], e]
    AppF e₁ e₂ -> parens [e₁, e₂]
    LetF bs e -> compileLet bs e
    CaseF e cs -> compileCase e cs
    SymF x -> fromText x
    ConF x -> fromText x
    LitF n -> decimal n
    _ -> error "Impossible!"

compileLet :: [Bind Builder] -> Builder -> Builder
compileLet bs e = parens ["letrec", parens (concatMap compileBind bs), e]

compileBind :: Bind Builder -> [Builder]
compileBind = \case
  Def x _ e -> [parens [fromText x, e]]
  Data x _ cs -> h : t
   where
    h = parens [fromText x, "null"]
    t =
      cs <&> \(y, _) ->
        parens
          [ fromText y
          , parens ["lambda", "$xs", parens ["cons", parens ["quote", fromText y], "$xs"]]
          ]

compileCase :: Builder -> [(Pat :@ a, Builder)] -> Builder
compileCase e cs = parens (h <> t)
 where
  h = ["match", e]
  t = [parens [compilePat p, e₂] | (p, e₂) <- cs]

compilePat :: Pat :@ a -> Builder
compilePat =
  unwrap >>> \case
    DestructF x ps ->
      parens
        $ ["list", parens ["quote", fromText x]]
        <> fmap compilePat ps
    BindF x -> fromText x
    IsLitF n -> decimal n
    WildF -> "_"

parens :: [Builder] -> Builder
parens xs = "(" <> mconcat (intersperse " " xs) <> ")"
