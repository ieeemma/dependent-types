{- |
This module defines the lexer for the language.
It is implemented using megaparsec. In other languages, this would be a
regex-based lexer, but Haskell doesn't commonly use regexes.

A string such as `let x = 1 in x` is lexed into a list of tokens:
```
[ Token TLet "let" (SourcePos …)
, Token TLower "x" (SourcePos …)
, Token TEquals "=" (SourcePos …)
, Token TNumber "1" (SourcePos …)
, Token TIn "in" (SourcePos …)
, Token TLower "x" (SourcePos …) ]
```
-}
module Parse.Lex where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, takeWhile1P), Parsec, SourcePos, choice, getSourcePos, many, manyTill, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, punctuationChar, string, upperChar)
import Text.Megaparsec.Char.Lexer (charLiteral)

-- | Lexer is a parser over `Text` that produces tokens.
type Lexer = Parsec Void Text

-- | Tokens store a kind, contents, and source location.
data Token = Token TokenKind Text SourcePos
  deriving (Eq, Ord)

-- | Simple enum of all token kinds.
data TokenKind
  = -- Punctuation
    TLambda
  | TArrow
  | TLParen
  | TRParen
  | TEquals
  | TColon
  | TUscore
  | -- Keywords
    TLet
  | TIn
  | TCase
  | TOf
  | TType
  | -- Atoms
    TLower
  | TUpper
  | TNumber
  | TString
  | TOperator
  | -- Whitespace
    TWs
  | TNl
  | -- Meta
    TOpen
  | TClose
  | TSep
  deriving (Show, Eq, Ord)

-- | Lex a string into a list of tokens.
tokens :: Lexer [Token]
tokens = many (choice $ mkToken <$> toks)

-- | List of token kinds and their parsers.
toks :: [(TokenKind, Lexer Text)]
toks =
  [ -- Punctuation
    (TLambda, string "λ" <|> string "\\")
  , (TArrow, string "->" <|> string "→")
  , (TLParen, string "(")
  , (TRParen, string ")")
  , (TEquals, string "=")
  , (TColon, string ":")
  , (TUscore, string "_")
  , -- Keywords
    (TLet, string "let")
  , (TIn, string "in")
  , (TCase, string "case")
  , (TOf, string "of")
  , (TType, string "Type")
  , -- Atoms
    (TLower, ident lowerChar alphaNumChar)
  , (TUpper, ident upperChar alphaNumChar)
  , (TNumber, int)
  , (TString, char '"' >> pack <$> manyTill charLiteral (char '"'))
  , (TOperator, ident punctuationChar punctuationChar)
  , -- Whitespace
    (TWs, takeWhile1P (Just "whitespace") (`elem` [' ', '\t']))
  , (TNl, string "\n")
  ]
 where
  ident p q = pack <$> liftA2 (:) p (many q) <* notFollowedBy q
  int = do
    op <- string "+" <|> string "-" <|> pure ""
    n <- takeWhile1P (Just "digit") isDigit
    pure (op <> n)

{- | Construct a token parser from a kind and parser.
This wraps the parser in a `Token` and adds the source position.
-}
mkToken :: (TokenKind, Lexer Text) -> Lexer Token
mkToken (k, p) = Token k <$> p <*> getSourcePos
