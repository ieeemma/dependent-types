module Test.Infer where

import Control.Arrow ((>>>))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (first)
import Data.Map (Map, alterF, toList)
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Error.Diagnose (Report, addFile, addReport, def, prettyDiagnostic)
import Prettyprinter (Pretty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

import Infer.Infer (Ctx (..), infer)
import Parse.Parse (file)
import Test.Parse (parseFrom)
import Util (files)

inferTests :: IO TestTree
inferTests = do
  -- Get all files and their contents in the Infer directory
  -- then pop the common file from the map.
  (common, fs') <- pop "common" <$> files "tests/Infer"
  pure $
    testGroup
      "Infer"
      [testCase p $ run p (common <> x) | (p, x) <- toList fs']

pop :: (Ord k) => k -> Map k v -> (v, Map k v)
pop k = alterF (,Nothing) k >>> first fromJust

run :: FilePath -> Text -> Assertion
run path src = case runExcept (runReaderT (infer tm) ctx) of
  Right _ -> pure ()
  Left e -> assertFailure (prettyError path src e)
 where
  tm = parseFrom file path src
  ctx = Ctx mempty mempty

-- prettyError :: FilePath -> Text -> String
prettyError :: (Pretty a) => FilePath -> Text -> Report a -> String
prettyError path src =
  addReport (addFile def path (unpack src))
    >>> prettyDiagnostic True 4
    >>> show
