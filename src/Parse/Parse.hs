{- | This module implements the bulk of the parser.
The grammar can be found in `docs/grammar`.

The main entrypoint is `parseSyntax`, which tokenizes then parses some
source code, producing either an error message or a syntax tree.
Trees are also annotated with source locations, the `Span` type,
which is used for error reporting and pretty-printing.
-}
module Parse.Parse (
  parseSyntax,
  Span (..),
  spanned,
  term,
) where

import Parse.Layout (layout)
import Parse.Lex (Token (..), TokenKind (..), tokens)
import Parse.Stream (TokenStream (..))
import Syntax

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.List.NonEmpty (fromList)
import Data.Set (singleton)
import Data.Text (Text, lines, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec hiding (Token, tokens)
import Prelude hiding (lines, pi)

-- | Parser over the custom `TokenStream` type.
type Parser = Parsec Void TokenStream

parseSyntax :: Text -> Text -> Parser a -> ([Token], Either Text a)
parseSyntax name src p = case parse tokens (unpack name) src of
  Left e -> (undefined, Left (pack $ errorBundlePretty e))
  Right ts -> case parse p (unpack name) (TokenStream (lines src) (layout ts)) of
    Left e -> (layout ts, Left (pack $ errorBundlePretty e))
    Right x -> (layout ts, Right x)

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
tok tk = token f set
 where
  f (Token tk' t _) | tk == tk' = Just t
  f _ = Nothing
  set = singleton $ Tokens $ fromList [Token tk "x" (SourcePos "" (mkPos 1) (mkPos 1))]

-- | Parse an integer literal.
int :: Parser Int
int = read . unpack <$> tok TNumber

-- | Parse a block of `p`s, consuming { ; } tokens.
block :: Parser a -> Parser [a]
block p = tok TOpen *> p `sepBy` tok TSep <* tok TClose

-- | Parse a whole term with operator precedence.
term :: Parser (ATm Span)
term =
  -- TODO: custom operator precedence for terms and patterns.
  makeExprParser grouping [[InfixL (pure app)], [InfixR (pure arr)]]
 where
  app x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< AppF x y
  arr x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< PiF "_" x y

grouping :: Parser (ATm Span)
grouping = try (spanned pi) <|> paren <|> atom
 where
  pi =
    PiF
      <$> (tok TLParen *> tok TLower)
      <*> (tok TColon *> term <* tok TRParen)
      <*> (tok TArrow *> term)
  paren = tok TLParen *> term <* tok TRParen

-- | Parse an atomic term.
atom :: Parser (ATm Span)
atom =
  spanned
    $ choice
      [ LamF
          <$> (tok TLambda *> tok TLower)
          <*> (tok TArrow *> term)
      , LetF
          <$> (tok TLet *> block bind)
          <*> (tok TIn *> term)
      , CaseF
          <$> (tok TCase *> term)
          <*> (tok TOf *> block alt)
      , LitF <$> int
      , UF <$ tok TType
      , SymF <$> tok TLower
      , ConF <$> tok TUpper
      ]
 where
  bind = (,,) <$> tok TLower <*> (tok TColon *> term) <*> (tok TEquals *> term)
  alt = (,) <$> pat <*> (tok TArrow *> term)

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
