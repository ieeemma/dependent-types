module Parse where

import Syntax

import Control.Monad (when)
import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR), makeExprParser)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space1, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char.Lexer qualified as L

-- See `docs/grammar` for further details.

type Parser = Parsec Void Text

ws :: Parser ()
ws =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol = L.symbol ws

-- | Given the start character of a name, parse the remainder.
name :: Parser Char -> Parser Sym
name p = try $ lexeme $ do
  c <- p
  cs <- many alphaNumChar <* notFollowedBy alphaNumChar
  let n = T.cons c (T.pack cs)
  when (n `elem` keywords) (fail "Keywords are not valid identifiers")
  pure n
 where
  keywords = ["let", "in", "case", "of", "Type"]

lower, upper :: Parser Sym
lower = name lowerChar
upper = name upperChar

-- | Parse a whole term with operator precedence.

-- TODO: custom operator precedence for terms and patterns.
tm :: Parser Tm
tm =
  makeExprParser
    atom
    [ [InfixL $ pure App]
    , [InfixR $ Pi "_" <$ symbol "->"]
    ]

-- | Parse an atomic part of a term, such as a lambda or int.
atom :: Parser Tm
atom =
  choice
    [ Pi
        <$> (symbol "(" *> lower)
        <*> (symbol ":" *> tm <* symbol ")")
        <*> (symbol "->" *> tm)
    , symbol "(" *> tm <* symbol ")"
    , Lam
        <$> (symbol "λ" *> lower)
        <*> (symbol "->" *> tm)
    , Let
        <$> (symbol "let" *> lower)
        <*> (symbol ":" *> tm)
        <*> (symbol "=" *> tm)
        <*> (symbol "in" *> tm)
    , Case
        <$> (symbol "case" *> tm)
        <*> (symbol "of" *> many alt)
    , Lit <$> lexeme decimal
    , U <$ symbol "Type"
    , Sym <$> lower
    , Con <$> upper
    ]
 where
  alt = (,) <$> pat <*> (symbol "->" *> tm)

-- | Parse a whole pattern term
pat :: Parser Pat
pat =
  choice
    [ Destruct
        <$> upper
        <*> many pat
    , Bind <$> lower
    , IsLit <$> lexeme decimal
    , Wild <$ symbol "_"
    ]
