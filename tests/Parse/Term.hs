{-# LANGUAGE DataKinds #-}

module Parse.Term where

import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Foldable (cata)
import Data.Text qualified as T

import Generic.Random (ConstrGen, constrGen, genericArbitraryRecG, uniform, withBaseCase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Text.Megaparsec (errorBundlePretty, parse)

import Arbitrary ()
import Parse.Layout (layout)
import Parse.Lex (tokens)
import Parse.Parse (tm)
import Parse.Stream (TokenStream (..))
import Pretty (render)
import Syntax

terms :: TestTree
terms =
  testGroup
    "Terms"
    [testProperty "Random" \x -> x === parse' (T.pack $ show (render x))]
 where
  parse' :: T.Text -> Tm Pat
  parse' x = case parse tokens "<test>" x of
    Left e -> error (errorBundlePretty e)
    Right ts -> case parse tm "<test>" (TokenStream (T.lines x) (layout ts)) of
      Left e -> error (errorBundlePretty e)
      Right x -> cata f x

  -- TODO: There *must* be a cleaner way to achieve this.
  -- Maybe `refix` and `embed`?
  -- For now, it works.
  -- Converts `Cofree` to `Fix` using a catamorphism.

  -- f :: CofreeF (TmF (Cofree PatF Span)) Span (Tm Pat) -> Tm Pat
  f (_ :< tm) = case tm of
    PiF x σ π -> Pi x σ π
    LamF x e -> Lam x e
    AppF e₁ e₂ -> App e₁ e₂
    LetF bs e -> Let bs e
    CaseF e ps -> Case e [(cata g p, e') | (p, e') <- ps]
    SymF x -> Sym x
    ConF x -> Con x
    LitF n -> Lit n
    UF -> U

  -- g :: CofreeF PatF Span Pat -> Pat
  g (_ :< pat) = case pat of
    DestructF x ps -> Destruct x ps
    BindF x -> Bind x
    IsLitF n -> IsLit n
    WildF -> Wild
