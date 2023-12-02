{-# LANGUAGE DataKinds #-}

module Parse.Term where

import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Data.Functor.Foldable (cata)
import Data.Text qualified as T
import Prettyprinter (pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, (===))
import Text.Megaparsec (eof, errorBundlePretty, parse)

import Arbitrary ()
import Parse (term, ws)
import Pretty ()
import Syntax

terms :: TestTree
terms =
  testGroup
    "Terms"
    [testProperty "Random" random]
 where
  random x =
    let src = show (pretty x)
     in case parse (ws *> term <* eof) "<test>" (T.pack src) of
          Left e -> error (src <> "\n\n" <> errorBundlePretty e)
          Right y -> x === cata f y

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
