module Main where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Text (unpack)
import Data.Text.IO qualified as TIO
import Error.Diagnose (addFile, addReport, def, defaultStyle, printDiagnostic, stderr)
import Text.Megaparsec (errorBundlePretty, parse)

import Infer.Infer (Ctx (..), infer)
import Infer.Quote (quote)
import Infer.Value
import Parse.Parse (file)
import Parse.Pretty ()

tys :: Env
tys = mempty

main :: IO ()
main = do
  let name = "examples/example"
  src <- TIO.readFile name
  case parse file name src of
    Left e -> putStrLn (errorBundlePretty e)
    Right t -> case runExcept (runReaderT (infer t) (Ctx tys mempty)) of
      Left e -> printDiagnostic stderr True True 4 defaultStyle (addReport (addFile def name (unpack src)) e)
      Right v -> print (quote v)
