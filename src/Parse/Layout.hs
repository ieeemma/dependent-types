module Parse.Layout where

import Control.Applicative (liftA2)
import Control.Monad.State (State, StateT, evalState, get, gets, modify, put, runStateT)
import Control.Monad.Trans (lift)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)), cons, fromList, uncons)
import Data.Maybe (fromJust)
import Parse.Lex (Token (Token), TokenKind (..))
import Text.Megaparsec (SourcePos, sourceColumn, unPos)

-- TODO: handle `in` properly!

{- | The toplevel layout monad.
Stores a non-empty stack of indentation levels.
-}
type Layout = State (NonEmpty Int)

{- | The inner stream monad.
Stores consumption of the input stream.
-}
type Stream = StateT [Token] Layout

-- | Replace indentation tokens with explicit layout tokens.
layout :: [Token] -> [Token]
layout ts = evalState (layout' ts) (fromList [0])

{- | Monadic layout algorithm.
Each 'handler' function (eg. block) runs in the Stream monad, allowing it
to consume the input stream.
-}
layout' :: [Token] -> Layout [Token]
layout' [] = end
layout' (t@(Token tk _ pos) : ts) = do
  (new, ts') <- flip runStateT ts $ case tk of
    TLet -> (t :) <$> block pos
    TOf -> (t :) <$> block pos
    TNl -> nl pos
    TWs -> pure []
    _ -> pure [t]
  (new <>) <$> layout' ts'

-- | At end of the input, close all blocks.
end :: Layout [Token]
end = do
  s <- get
  pure (replicate (length s - 1) (close undefined))

-- | After a token that starts a block, eg `let` or `of`, insert { ; }.
block :: SourcePos -> Stream [Token]
block pos =
  cmp >>= \case
    Just (i, GT) -> do
      lift (modify (cons i))
      pure [open pos]
    Just _ -> do
      pure [open pos, close pos]
    _ -> pure []

-- | After a newline, insert either ; or }.
nl :: SourcePos -> Stream [Token]
nl pos =
  cmp >>= \case
    Just (_, LT) -> do
      lift (modify $ fromJust . snd . uncons)
      pure [close pos]
    Just (_, EQ) -> do
      pure [sep pos]
    _ -> pure []

-- | Compare the current indentation level to the next non-whitespace token.
cmp :: Stream (Maybe (Int, Ordering))
cmp =
  liftA2 (,) ref (lift get) <&> \case
    (Just i, j :| _) -> Just (i, compare i j)
    _ -> Nothing

-- | Find the indentation level of the next non-whitespace token.
ref :: Stream (Maybe Int)
ref =
  gets (dropWhile whitespace) >>= \case
    [] -> pure Nothing
    ts@(Token _ _ pos : _) -> do
      put ts
      pure (Just (unPos (sourceColumn pos) - 1))

whitespace :: Token -> Bool
whitespace (Token tk _ _) = case tk of
  TWs -> True
  TNl -> True
  _ -> False

open, close, sep :: SourcePos -> Token
open = Token TOpen "{"
close = Token TClose "}"
sep = Token TSep ";"
