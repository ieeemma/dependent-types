module Parse.Pretty where

import Control.Applicative (liftA2)
import Data.Functor.Foldable (cata, para)
import Prettyprinter (Doc, Pretty (..), hsep, indent, line, vsep, (<+>))

import Syntax

instance (Pretty t) => Pretty (Tl t) where
  pretty = cata \case
    DefF x σ e -> pretty x <> ":" <+> pretty σ <+> "=" <+> pretty e
    DataF x σ cs -> "data" <+> pretty x <+> ":" <+> pretty σ <+> block (con <$> cs)
   where
    con (x, σ) = pretty x <+> ":" <+> pretty σ

instance (Pretty p) => Pretty (Tm p) where
  pretty = para \case
    PiF "_" σ π -> paren atom σ <+> "->" <+> paren (liftA2 (||) atom arrow) π
    PiF x (_, σ) (_, π) -> "(" <> pretty x <+> ":" <+> σ <> ")" <+> "->" <+> π
    LamF x (_, e) -> "λ" <> pretty x <+> "->" <+> e
    AppF e₁ e₂ -> paren (liftA2 (||) atom app) e₁ <+> paren atom e₂
    LetF bs (_, e) -> "let" <+> block (bind <$> bs) <+> "in" <+> e
    CaseF (_, e) ps -> "case" <+> e <+> "of" <+> block (alt <$> ps)
    SymF x -> pretty x
    ConF x -> pretty x
    LitF n -> pretty n
    UF -> "Type"
   where
    bind (x, (_, σ), (_, e)) = pretty x <> ":" <+> σ <+> "=" <+> e
    alt (p, (_, e)) = pretty p <+> "->" <+> e

    arrow = \case
      Pi "_" _ _ -> True
      _ -> False
    app = \case
      App{} -> True
      _ -> False
    atom = \case
      Pi{} -> False
      Lam{} -> False
      App{} -> False
      Let{} -> False
      Case{} -> False
      _ -> True

instance Pretty Pat where
  pretty = para \case
    DestructF x ps -> pretty x <+> hsep (paren atom <$> ps)
    BindF x -> pretty x
    IsLitF n -> pretty n
    WildF -> "_"
   where
    atom = \case
      Destruct{} -> False
      _ -> True

-- | Parenthesize a `Doc` if a predicate is false.
paren :: (a -> Bool) -> (a, Doc ann) -> Doc ann
paren p (e, x) = if not (p e) then "(" <> x <> ")" else x

-- | Line-separate and indent a list of `Doc`s.
block :: [Doc ann] -> Doc ann
block xs = "{" <> line <> vsep [indent 2 x <> ";" | x <- xs] <> "}"
