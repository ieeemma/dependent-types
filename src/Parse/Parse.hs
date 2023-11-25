module Parse.Parse where

import Parse.Lex
import Parse.Stream
import Syntax

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec hiding (tokens)
import Prelude hiding (pi)

-- | Parser over the custom `TokenStream` type.
type Parser = Parsec Void TokenStream

-- | Source location tracking
data Span = Span SourcePos SourcePos

-- | Wrap a parser in its source location span using a `Cofree`.
spanned :: Parser (f (Cofree f Span)) -> Parser (Cofree f Span)
spanned p = do
  l <- getSourcePos
  x <- p
  r <- getSourcePos
  pure (Span l r :< x)

-- | Parse a token of a given kind.
tok :: TokenKind -> Parser Text
tok tk = token f mempty -- TODO: fill in set
 where
  f (Token tk' t _) | tk == tk' = Just t
  f _ = Nothing

-- | Parse an integer literal.
int :: Parser Int
int = read . unpack <$> tok TNumber

-- | Parse a block of `p`s, consuming { ; } tokens.
block :: Parser a -> Parser [a]
block p = tok TOpen *> p `sepBy` tok TSep <* tok TClose

-- | Parse a whole term with operator precedence.
tm :: Parser (ATm Span)
tm =
  -- TODO: custom operator precedence for terms and patterns.
  makeExprParser grouping [[InfixL (pure app)], [InfixR (pure arr)]]
 where
  app x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< AppF x y
  arr x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< PiF "_" x y

grouping :: Parser (ATm Span)
grouping = spanned pi <|> paren <|> atom
 where
  pi =
    PiF
      <$> (tok TLParen *> tok TLower)
      <*> (tok TColon *> tm <* tok TRParen)
      <*> (tok TArrow *> tm)
  paren = tok TLParen *> tm <* tok TRParen

-- | Parse an atomic term.
atom :: Parser (ATm Span)
atom =
  spanned
    $ choice
      [ LamF
          <$> (tok TLambda *> tok TLower)
          <*> (tok TArrow *> tm)
      , LetF
          <$> (tok TLet *> block bind)
          <*> (tok TIn *> tm)
      , CaseF
          <$> (tok TCase *> tm)
          <*> (tok TOf *> block alt)
      , LitF <$> int
      , UF <$ tok TType
      , SymF <$> tok TLower
      , ConF <$> tok TUpper
      ]
 where
  bind = (,,) <$> tok TLower <*> (tok TColon *> tm) <*> (tok TEquals *> tm)
  alt = (,) <$> pat <*> (tok TArrow *> tm)

-- | Parse a pattern.
pat :: Parser (APat Span)
pat =
  spanned
    $ choice
      [ DestructF
          <$> tok TUpper
          <*> many pat
      , BindF <$> tok TLower
      , IsLitF <$> int
      , WildF <$ tok TUscore
      ]
