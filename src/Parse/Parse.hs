{- | This module implements the  parser.
The grammar can be found in `docs/grammar`.
-}
module Parse.Parse where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space1, string, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (pi)

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import Syntax

-- | Parse `Text` without a custom state (`Void`).
type Parser = Parsec Void Text

-- | Source location tracking
data Span = Span SourcePos SourcePos

-- | Wrap a parser in its source location span using a `Cofree`.
spanned :: Parser (f (Cofree f Span)) -> Parser (Cofree f Span)
spanned p = do
  l <- getSourcePos
  x <- p
  r <- getSourcePos
  pure (Span l r :< x)

-- | Consume comments and whitespace.
ws :: Parser ()
ws =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")

-- | Consume comments and whitespace after a lexeme.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

-- | Parse a literal string with trailing whitespace.
symbol :: Text -> Parser Text
symbol = lexeme . string

-- | Parse a block of `p`s, consuming { ; } tokens.
block :: Parser a -> Parser [a]
block p = symbol "{" *> p `sepEndBy` symbol ";" <* symbol "}"

-- | Parse an identifier with start and rest parsers.
ident :: Parser Char -> Parser Char -> Parser Text
ident p ps = lexeme $ try $ do
  x <- p
  xs <- many ps
  let s = pack (x : xs)
  when (s `elem` keywords) $ fail ("keyword " <> show s <> " cannot be an identifier")
  pure s
 where
  keywords = ["let", "in", "case", "of", "Type", "data"]

-- | Parse lowercase and uppercase identifiers.
lower, upper :: Parser Text
lower = ident lowerChar alphaNumChar
upper = ident upperChar alphaNumChar

-- | Parse an integer.
int :: Parser Int
int = lexeme $ try $ L.signed (pure ()) L.decimal

-- | Parse a toplevel definition.
tl :: Parser (ATl Span)
tl =
  spanned
    $ choice
      [ DataF <$> (symbol "data" *> upper) <*> (symbol ":" *> term) <*> block con
      , DefF <$> lower <*> (symbol ":" *> term) <*> (symbol "=" *> term)
      ]
 where
  con = (,) <$> upper <*> (symbol ":" *> term)

-- | Parse a whole term with operator precedence.
term :: Parser (ATm Span)
term =
  -- TODO: custom operator precedence for terms and patterns.
  makeExprParser grouping [[InfixL (pure app)], [InfixR (pure arr)]]
 where
  app x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< AppF x y
  arr x@(Span l _ :< _) y@(Span _ r :< _) = Span l r :< PiF "_" x y

  -- Parse a term grouping.
  grouping = try (spanned pi) <|> paren <|> atom
   where
    pi =
      PiF
        <$> (symbol "(" *> lower)
        <*> (symbol ":" *> term <* symbol ")")
        <*> (symbol "->" *> term)
    paren = between (symbol "(") (symbol ")") term

  -- Parse an atomic term.
  atom =
    spanned
      $ choice
        [ LamF
            <$> (symbol "λ" *> lower)
            <*> (symbol "->" *> term)
        , LetF
            <$> (symbol "let" *> block bind)
            <*> (symbol "in" *> term)
        , CaseF
            <$> (symbol "case" *> term)
            <*> (symbol "of" *> block alt)
        , LitF <$> int
        , UF <$ symbol "Type"
        , SymF <$> lower
        , ConF <$> upper
        ]

  bind = (,,) <$> lower <*> (symbol ":" *> term) <*> (symbol "=" *> term)
  alt = (,) <$> pat <*> (symbol "->" *> term)

-- | Parse a pattern.
pat :: Parser (APat Span)
pat = paren <|> atom
 where
  paren = between (symbol "(") (symbol ")") pat
  atom =
    spanned
      $ choice
        [ DestructF
            <$> upper
            <*> many pat
        , BindF <$> lower
        , IsLitF <$> int
        , WildF <$ symbol "_"
        ]
