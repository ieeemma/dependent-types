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
    Lambda
  | Arrow
  | LParen
  | RParen
  | Equals
  | Colon
  | Uscore
  | -- Keywords
    Let
  | In
  | Case
  | Of
  | Type
  | -- Atoms
    Lower
  | Upper
  | Number
  | String
  | Operator
  | -- Whitespace
    Ws
  | Nl
  | -- Meta
    Open
  | Close
  | Sep

tokens :: Lexer [Token]
tokens = many (choice $ mkToken <$> toks)

toks :: [(TokenKind, Lexer Text)]
toks =
  [ -- Punctuation
    (Lambda, string "λ" <|> string "\\")
  , (Arrow, string "->" <|> string "→")
  , (LParen, string "(")
  , (RParen, string ")")
  , (Equals, string "=")
  , (Colon, string ":")
  , (Uscore, string "_")
  , -- Keywords
    (Let, string "let")
  , (In, string "in")
  , (Case, string "case")
  , (Of, string "of")
  , (Type, string "Type")
  , -- Atoms
    (Lower, ident lowerChar alphaNumChar)
  , (Upper, ident upperChar alphaNumChar)
  , (Number, takeWhile1P (Just "digit") isDigit)
  , (String, char '"' >> pack <$> manyTill charLiteral (char '"'))
  , (Operator, ident punctuationChar punctuationChar)
  , -- Whitespace
    (Ws, takeWhile1P (Just "whitespace") (`elem` [' ', '\t']))
  , (Nl, string "\n")
  ]

ident :: Lexer Char -> Lexer Char -> Lexer Text
ident p q = pack <$> liftA2 (:) p (many q) <* notFollowedBy q

mkToken :: (TokenKind, Lexer Text) -> Lexer Token
mkToken (k, p) = Token k <$> p <*> getSourcePos
