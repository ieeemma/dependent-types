module Infer.Error where

import Control.Comonad (extract)
import Control.Monad.Except (Except, throwError)
import Data.Text (Text)
import Error.Diagnose (Marker (This), Position (..), Report, err)
import Prelude hiding (lookup)

import Infer.Quote (quote)
import Infer.Value (Val)
import Parse.Parse (Span (..))
import Parse.Pretty (render)
import Syntax ((:@))
import Util (fromSpan)

-- | Produce a marker for a type and message.
marker :: Val :@ Span -> (Text -> Text) -> (Position, Marker Text)
marker τ f = (fromSpan (extract τ), This (f $ render (quote τ)))

-- | Produce a type error from a message and a list of types and messages.
typeError :: Text -> [(Position, Marker Text)] -> Except (Report Text) a
typeError msg ms = throwError (err Nothing msg ms [])
