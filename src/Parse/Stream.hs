{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This module defines the token stream type used by the parser,
allowing us to use the custom `Token` type with megaparsec.
The `Stream` typeclass defined by megaparsec has functions for taking data
from the stream and chunking it.
In addition, the `VisualStream` and `TraversalStream` typeclasses allow us to
pretty-print the stream and update the line/column state.
These both allow for pretty error messages.
-}
module Parse.Stream where

import Data.List.NonEmpty (toList)
import Data.Proxy
import Data.Text qualified as T
import Text.Megaparsec

import Parse.Lex qualified as L

{- | The stream of tokens fed to megaparsec.
Stores the remaining token and the original source lines for error reporting.
-}
data TokenStream = TokenStream [T.Text] [L.Token]

-- This is largely an adaptation of the `[a]` instance found
-- [here](https://hackage.haskell.org/package/megaparsec-9.5.0/docs/src/Text.Megaparsec.Stream.html#line-135).
instance Stream TokenStream where
  type Token TokenStream = L.Token
  type Tokens TokenStream = [L.Token]
  tokenToChunk :: Proxy TokenStream -> Token TokenStream -> Tokens TokenStream
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  take1_ (TokenStream src inp) = case inp of
    [] -> Nothing
    (t : ts) -> Just (t, TokenStream src ts)
  takeN_ n s@(TokenStream src inp)
    | n <= 0 = Just ([], s)
    | null inp = Nothing
    | otherwise = let (t, ts) = splitAt n inp in Just (t, TokenStream src ts)
  takeWhile_ p (TokenStream src inp) =
    let (l, r) = span p inp in (l, TokenStream src r)

-- Deriving `VisualStream` allows us to pretty-print the token stream.
-- This is useful for debugging and error messages.
instance VisualStream TokenStream where
  showTokens _ ts = unwords [tail (show tk) | L.Token tk _ _ <- toList ts]
  tokensLength _ ts = sum [T.length t | L.Token _ t _ <- toList ts]

-- Deriving `TraversalStream` allows us to update the megaparsec line/column state
-- with token positions.
-- Also yields lines of the original source for error messages.
-- Reads `off` tokens ahead and computes new info.
instance TraversableStream TokenStream where
  reachOffset off PosState{..} =
    let TokenStream src ts = pstateInput
        ts' = drop off ts
        pos = case ts' of
          [] -> tpos (last ts)
          (t : _) -> tpos t
        src' = drop (pline pos - pline pstateSourcePos) src
        line = case src' of [] -> Nothing; (l : _) -> Just (T.unpack l)
        new =
          PosState
            { pstateInput = TokenStream src' ts'
            , pstateOffset = off
            , pstateSourcePos = pos
            , pstateTabWidth = pstateTabWidth
            , -- TODO: What is this field??
              pstateLinePrefix = pstateLinePrefix
            }
     in (line, new)
   where
    tpos (L.Token _ _ p) = p
    pline p = unPos (sourceLine p)
