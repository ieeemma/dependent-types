{-# LANGUAGE UndecidableInstances #-}

module Pretty where

import Control.Arrow ((>>>))
import Control.Comonad.Trans.Cofree (CofreeF (..))
import Data.Functor.Foldable (Base, Recursive, para, project)
import Prettyprinter (Doc, hsep, indent, line, pretty, vsep, (<+>))
import Syntax

class Render f where
  -- | Given a structure containing `Doc`s, render it to a `Doc`.
  step :: f (Doc ann) -> Doc ann

  -- | Return the precedence of a structure for parenthesization.
  prec :: f a -> Int

-- | Render a `Render`able structure to a `Doc`, inserting parentheses.
render :: (Recursive a, Render (Base a)) => a -> Doc ann
render = para (parens >>> step)
 where
  parens x = paren (prec x) <$> x
  paren n (y, d)
    | prec (project y) > n = "(" <> d <> ")"
    | otherwise = d

-- instance {-# OVERLAPS #-} (Recursive a, Render (Base a)) => Show a where
--   show = show . render

instance (Render f) => Render (CofreeF f a) where
  step (_ :< xs) = step xs
  prec (_ :< xs) = prec xs

instance (Recursive p, Render (Base p)) => Render (TmF p) where
  step = \case
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
   where
    bind (x, σ, e) = pretty x <> ":" <+> σ <+> "=" <+> e
    alt (p, e) = render p <+> "->" <+> e

  prec = \case
    PiF "_" _ _ -> 10
    PiF{} -> 2
    LamF{} -> 10
    AppF{} -> 1
    LetF{} -> 10
    CaseF{} -> 10
    _ -> 0

instance Render PatF where
  step = \case
    DestructF x ps -> pretty x <+> hsep ps
    BindF x -> pretty x
    IsLitF n -> pretty n
    WildF -> "_"

  prec = \case
    DestructF{} -> 10
    _ -> 0

-- | Line-separate and indent a list of `Doc`s.
block :: [Doc ann] -> Doc ann
block = (line <>) . vsep . map (indent 2)
