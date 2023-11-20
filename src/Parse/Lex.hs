module Parse.Lex where

import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy, takeWhile1P), Parsec, SourcePos, choice, getSourcePos, many, manyTill, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, punctuationChar, string, upperChar)
import Text.Megaparsec.Char.Lexer (charLiteral)

type Lexer = Parsec Void Text

data Token = Token TokenKind Text SourcePos

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
  deriving (Show, Eq)

tokens :: Lexer [Token]
tokens = many (choice $ mkToken <$> toks)

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
  , (TNumber, takeWhile1P (Just "digit") isDigit)
  , (TString, char '"' >> pack <$> manyTill charLiteral (char '"'))
  , (TOperator, ident punctuationChar punctuationChar)
  , -- Whitespace
    (TWs, takeWhile1P (Just "whitespace") (`elem` [' ', '\t']))
  , (TNl, string "\n")
  ]

ident :: Lexer Char -> Lexer Char -> Lexer Text
ident p q = pack <$> liftA2 (:) p (many q) <* notFollowedBy q

mkToken :: (TokenKind, Lexer Text) -> Lexer Token
mkToken (k, p) = Token k <$> p <*> getSourcePos
