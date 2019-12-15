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

-- | Returns the set of words which can be spelled using the given letters.
allowedWords :: FilePath -> Multiset Char -> IO (Set Text)
allowedWords path letters = Set.fromList . fmap T.pack . filter isAllowed <$> candidates where
  candidates = lines <$> readFile path
  isAllowed word = Multiset.fromList word `Multiset.isSubsetOf` letters

main :: IO ()
main = do
  (flags, _) <- FA.parseSystemFlagsOrDie flagsParser
  let
    entries =
      [ Entry "food" Horizontal 0
      , Entry "feed" Horizontal (YX 3 0)
      , Entry "hi" Horizontal (YX 0 6)
      , Entry "hi" Horizontal (YX 3 6)
      , Entry "oie" Vertical (YX 0 1 )]
  case displayEntries entries of
    Left conflict -> print "conflict"
    Right bs -> BS.putStrLn bs
