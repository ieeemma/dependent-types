module Parse.Parse where

import Parse.Lex
import Parse.Stream (TokenStream)
import Syntax

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec hiding (tokens)

-- | Parser over the custom `TokenStream` type.
type Parser = Parsec Void TokenStream

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
tm :: Parser Tm
tm =
  -- TODO: custom operator precedence for terms and patterns.
  makeExprParser
    atom
    [ [InfixL $ pure App]
    , [InfixR $ Pi "_" <$ tok TArrow]
    ]

-- | Parse an atomic term.
atom :: Parser Tm
atom =
  choice
    [ Pi
        <$> (tok TLParen *> tok TLower)
        <*> (tok TColon *> tm <* tok TRParen)
        <*> (tok TArrow *> tm)
    , tok TLParen *> tm <* tok TRParen
    , Lam
        <$> (tok TLambda *> tok TLower)
        <*> (tok TArrow *> tm)
    , Let
        <$> (tok TLet *> block bind)
        <*> (tok TIn *> tm)
    , Case
        <$> (tok TCase *> tm)
        <*> (tok TOf *> block alt)
    , Lit <$> int
    , U <$ tok TType
    , Sym <$> tok TLower
    , Con <$> tok TUpper
    ]
 where
  bind = (,,) <$> tok TLower <*> (tok TColon *> tm) <*> (tok TEquals *> tm)
  alt = (,) <$> pat <*> (tok TArrow *> tm)

-- | Parse a pattern.
pat :: Parser Pat
pat =
  choice
    [ Destruct
        <$> tok TUpper
        <*> many pat
    , Bind <$> tok TLower
    , IsLit <$> int
    , Wild <$ tok TUscore
    ]
