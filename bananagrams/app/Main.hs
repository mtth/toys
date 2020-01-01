{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- To use it:
--
--   stack bananagrams.hs --chars=aaboajewq
module Main where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS
import Data.Geometry.YX (YX(YX))
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Flags.Applicative as FA

import Bananagrams

-- | The command's input flags.
data Flags
  = Flags
    { dictionaryPath :: FilePath
    , availableLetters :: Multiset Char }
    deriving Show

flagsParser :: FA.FlagsParser Flags
flagsParser =
  let stringVal = fmap T.unpack <$> FA.textVal
  in Flags
    <$> (FA.flag stringVal "dictionary_path" "path to words" <|> pure "/usr/share/dict/words")
    <*> (Multiset.fromList <$> FA.flag stringVal "letters" "available letters")

readDict :: FilePath -> IO Dictionary
readDict path = newDictionary . filter ((> 2) . T.length) . T.lines <$> T.readFile path

main :: IO ()
main = do
  (flags, _) <- FA.parseSystemFlagsOrDie flagsParser
  dict <- readDict $ dictionaryPath flags
  solve dict (availableLetters flags) >>= \case
    Just entries -> case displayEntries entries of
      Left conflict -> print "conflict"
      Right bs -> BS.putStrLn bs
    Nothing -> putStrLn "unsolved"
