{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- To use it:
--
--   stack bananagrams.hs --chars=aaboajewq
module Main where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Flags.Applicative as FA

import Bananagrams

-- | The command's input flags.
data Flags
  = Flags
    { dictionaryPath :: FilePath
    , availableLetters :: Multiset Char
    , logLevel :: Severity }
    deriving Show

flagsParser :: FA.FlagsParser Flags
flagsParser =
  let stringVal = fmap T.unpack <$> FA.textVal
  in Flags
    <$> (FA.flag stringVal "dictionary_file" "path to words" <|> pure "/usr/share/dict/words")
    <*> (Multiset.fromList <$> FA.flag stringVal "letters" "available letters")
    <*> (FA.flag FA.enumVal "log_level" "log level" <|> pure Notice)

readDict :: FilePath -> IO Dictionary
readDict path = newDictionary . filter ((> 1) . T.length) . T.lines <$> T.readFile path

main :: IO ()
main = do
  (flags, _) <- FA.parseSystemFlagsOrDie flagsParser
  dict <- readDict $ dictionaryPath flags
  solve (logLevel flags) dict (availableLetters flags) >>= \case
    Just entries -> case displayEntries entries of
      Right bs -> BS.putStrLn bs
      _ -> error "conflict"
    Nothing -> putStrLn "unsolved"
