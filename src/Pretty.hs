module Pretty where

import Control.Arrow ((>>>))
import Data.Functor.Foldable (Base, Recursive, para, project)
import Prettyprinter (Doc, Pretty, hsep, indent, line, pretty, vsep, (<+>))
import Syntax

-- | Return the precedence of a type, low is higher.
class Prec a where
  prec :: a -> Int

instance Prec (TmF a) where
  prec = \case
    PiF "_" _ _ -> 10
    PiF{} -> 2
    LamF{} -> 10
    AppF{} -> 1
    LetF{} -> 10
    CaseF{} -> 10
    _ -> 0

instance Prec (PatF a) where
  prec = \case
    DestructF{} -> 10
    _ -> 0

-- | Inject parenthesis to children of a `Recursive` with lower precedence.
parens :: (Recursive t, Prec (Base t t), Prec (Base t (t, Doc ann))) => Base t (t, Doc ann) -> Base t (Doc ann)
-- TODO: this could be simplified by rolling the `Base` constraint
-- into `Prec`?
parens x = fmap (paren $ prec x) x
 where
  paren n (y, d)
    | prec (project y) > n = "(" <> d <> ")"
    | otherwise = d

-- | Indent sub-terms such as `let` and `case`.
block :: [Doc ann] -> Doc ann
block = (line <>) . vsep . map (indent 2)

instance Pretty Tm where
  pretty = para (parens >>> f)
   where
    f :: Base Tm (Doc ann) -> Doc ann
    f = \case
      PiF "_" σ π -> σ <+> "->" <+> π
      PiF x σ π -> "(" <> pretty x <> ":" <+> σ <> ")" <+> "->" <+> π
      LamF x e -> "λ" <> pretty x <+> "->" <+> e
      AppF e₁ e₂ -> e₁ <+> e₂
      LetF bs e -> "let" <> block (bind <$> bs) <+> "in" <+> e
      CaseF e ps -> "case" <+> e <+> "of" <> block (alt <$> ps)
      SymF x -> pretty x
      ConF x -> pretty x
      LitF n -> pretty n
      UF -> "Type"

    bind (x, σ, e) = pretty x <> ":" <+> σ <+> "=" <+> e
    alt (p, e) = pretty p <+> "->" <+> e

instance Pretty Pat where
  pretty = para (parens >>> f)
   where
    f :: Base Pat (Doc ann) -> Doc ann
    f = \case
      DestructF x ps -> pretty x <+> hsep ps
      BindF x -> pretty x
      IsLitF n -> pretty n
      WildF -> "_"

instance Show Tm where
  show = show . pretty
