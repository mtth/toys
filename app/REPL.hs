{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal REPL implementation.
--
-- Usage: stack run [PATH ...]
--
-- Bindings defined in each PATH (one per line) will be loaded into the REPL at startup.
module Main where

import HindleyMilner

import Control.Monad (forever)
import Control.Monad.State (evalStateT, get)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldlM)
import qualified Data.Text.IO as T
import qualified Text.Megaparsec as P
import System.Environment (getArgs)
import System.IO (IOMode(..), hFlush, hIsEOF, stdout, withFile)

loadEnv :: Env -> FilePath -> IO Env
loadEnv env fp = withFile fp ReadMode $ \h -> evalStateT (go h) env where
  go h = liftIO (hIsEOF h) >>= \case
    True -> get
    False -> liftIO (T.hGetLine h) >>= interpret >> go h

main :: IO ()
main = getEnv >>= evalStateT (forever go) where
  getEnv = getArgs >>= foldlM loadEnv defaultEnv
  go = do
    line <- liftIO $ T.putStr "hm> " >> hFlush stdout >> T.getLine
    result <- interpret line
    liftIO $ case result of
      Computation (LitV lit) -> T.putStrLn $ displayLit lit
      Computation (ClosureV _) -> putStrLn "<closure>"
      Computation (UndefinedV) -> putStrLn "<undefined>"
      BoundIdentifier iden tp -> T.putStrLn $ iden <> " :: " <> displayType tp
      InvalidSyntax err -> putStrLn $ "syntax error: " <> P.errorBundlePretty err
      InvalidType err -> T.putStrLn $ "type error: " <> displayTypeError err
