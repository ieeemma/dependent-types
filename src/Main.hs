module Main where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Map (toList)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as TIO
import Error.Diagnose (Report, addFile, addReport, def, defaultStyle, printDiagnostic, stderr)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (dropExtension)
import System.Process (readProcess)
import Text.Megaparsec (errorBundlePretty, parse)

import Compile.Compile (compile)
import Infer.Infer (Ctx (..), infer)
import Parse.Parse (Span, file, mainTerm)
import Syntax (Bind, Tm, (:@))
import Util (files)

-- pure

main :: IO ()
main =
  getArgs >>= \case
    [name] -> do
      -- Read the source file
      src <- TIO.readFile name
      -- Read stdlib directory
      std <- files "stdlib"
      -- Parse all files
      binds <- concat <$> uncurry parseFile `traverse` (toList std <> [(name, src)])
      let tm = mainTerm binds
      -- Type check the term, exit on type error
      case runExcept (runReaderT (infer tm) (Ctx mempty mempty)) of
        Left e -> typeError ((name, src) : toList std) e *> exitFailure
        Right _ -> pure ()
      -- Write the compiled term to disk using the same name as the source file
      let out = dropExtension name <> ".rkt"
      TIO.writeFile out ("#lang racket\n" <> compile tm)
      -- Run the racket formatter, for debugging purposes
      _ <- readProcess "raco" ["fmt", "-i", out] ""
      pure ()
    _ -> do
      -- Usage message
      p <- getProgName
      putStrLn ("usage: " <> p <> " <file>")

parseFile :: FilePath -> Text -> IO [Bind (Tm :@ Span)]
parseFile p src = case parse file p src of
  Left e -> putStrLn (errorBundlePretty e) *> exitFailure
  Right x -> pure x

typeError :: [(FilePath, Text)] -> Report Text -> IO ()
typeError fs e =
  printDiagnostic
    stderr
    True
    True
    4
    defaultStyle
    (addReport (f fs) e)
 where
  f [] = def
  f ((name, src) : fs) = addFile (f fs) name (unpack src)
