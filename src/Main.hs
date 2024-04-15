module Main where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as TIO
import Error.Diagnose (Report, addFile, addReport, def, defaultStyle, printDiagnostic, stderr)
import System.Environment (getArgs, getProgName)
import System.FilePath (dropExtension)
import System.Process (readProcess)
import Text.Megaparsec (errorBundlePretty, parse)

import Compile.Compile (compile)
import Infer.Infer (Ctx (..), infer)
import Parse.Parse (file)
import Parse.Pretty ()
import System.Exit (exitFailure)

main :: IO ()
main =
  getArgs >>= \case
    [name] -> do
      -- Read the source file
      src <- TIO.readFile name
      -- Parse the source, exit on parse error
      tm <- case parse file name src of
        Left e -> putStrLn (errorBundlePretty e) *> exitFailure
        Right x -> pure x
      -- Type check the term, exit on type error
      case runExcept (runReaderT (infer tm) (Ctx mempty mempty)) of
        Left e -> typeError name src e *> exitFailure
        Right _ -> pure ()
      -- Write the compiled term to disk using the same name as the source file
      let out = dropExtension name <> ".rkt"
      TIO.writeFile out (compile tm)
      -- Run the racket formatter, for debugging purposes
      _ <- readProcess "raco" ["fmt", "-i", out] ""
      pure ()
    _ -> do
      -- Usage message
      p <- getProgName
      putStrLn ("usage: " <> p <> " <file>")

typeError :: FilePath -> Text -> Report Text -> IO ()
typeError name src e =
  printDiagnostic
    stderr
    True
    True
    4
    defaultStyle
    (addReport (addFile def name (unpack src)) e)
