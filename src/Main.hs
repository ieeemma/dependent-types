module Main where

import Control.Comonad.Cofree (Cofree ((:<)))
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
import Parse.Parse (Span, file)
import Syntax (Bind, Tm, TmF (..), (:@))
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
      let tm = error "TODO" :< LetF binds (error "TODO" :< SymF "main")
      -- Type check the term, exit on type error
      case runExcept (runReaderT (infer tm) (Ctx mempty mempty)) of
        Left e -> typeError name src e *> exitFailure
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

typeError :: FilePath -> Text -> Report Text -> IO ()
typeError name src e =
  printDiagnostic
    stderr
    True
    True
    4
    defaultStyle
    (addReport (addFile def name (unpack src)) e)
