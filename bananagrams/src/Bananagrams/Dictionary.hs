-- | Bananagrams word selection
module Bananagrams.Dictionary (
  Dictionary, newDictionary,
  firstWords, matchingWords
) where

import Control.Monad.ST (runST)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Intro as Vector

newtype Dictionary = Dictionary (Vector Text)

newDictionary :: [Text] -> Dictionary
newDictionary = Dictionary . Vector.fromList

spellable :: Multiset Char -> Text -> Bool
spellable chars word = Multiset.fromList (T.unpack word) `Multiset.isSubsetOf` chars

sortByDescLength :: Vector Text -> Vector Text
sortByDescLength vec = runST $ do
  vec' <- Vector.thaw vec
  Vector.sortBy (comparing (\txt -> (- T.length txt))) vec'
  Vector.unsafeFreeze vec'

-- | Returns the best words to use first given the allowed characters.
firstWords :: Dictionary -> Multiset Char -> [Text]
firstWords (Dictionary words) chars =
  let vec = Vector.filter (spellable chars) words
  in Vector.toList $ sortByDescLength vec

matchingWords :: Dictionary -> Multiset Char -> Map Int Char -> [Text]
matchingWords = undefined
