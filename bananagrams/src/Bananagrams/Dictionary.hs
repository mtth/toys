{-# LANGUAGE TupleSections #-}

-- | Bananagrams word selection
module Bananagrams.Dictionary (
  Dictionary, newDictionary,
  Hand, firstWords, matchingWords
) where

import Control.Monad.ST (runST)
import Data.Foldable (foldl')
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

data Item = Item { itemTxt :: !Text, itemVec :: !(Vector Char), itemMset :: !(Multiset Char) }

newItem :: Text -> Item
newItem txt = let chars = T.unpack txt in Item txt (Vector.fromList chars) (Multiset.fromList chars)

-- | A bag of allowed words.
newtype Dictionary = Dictionary (Vector Item)

sortByDescLength :: Vector Item -> Vector Item
sortByDescLength vec = runST $ do
  vec' <- Vector.thaw vec
  Vector.sortBy (comparing (\item -> (- T.length (itemTxt item)))) vec'
  Vector.unsafeFreeze vec'

-- | Generates a 'Dictionary' from a list of words.
newDictionary :: [Text] -> Dictionary
newDictionary = Dictionary . sortByDescLength . Vector.fromList . fmap newItem

spellable :: Hand -> Item -> Bool
spellable chars (Item _ _ mset) = mset `Multiset.isSubsetOf` chars

-- | A convenience alias for the characters in hand (to be placed on the grid).
type Hand = Multiset Char

-- | Returns the best words to use first given the allowed characters.
firstWords :: Dictionary -> Hand -> [Text]
firstWords (Dictionary items) chars =
  fmap itemTxt $ Vector.toList $ Vector.filter (spellable chars) items

itemOffsets :: Hand -> Map Int Char -> (Int, Int) -> Item -> [Int]
itemOffsets hand chars (leftBound, rightBound) (Item _ vec mset) =
  let
    len = Vector.length vec
    minOffset = max 0 (len - 1 - rightBound)
    maxOffset = min (len - 1) leftBound
    matches k = all (\(i, char) -> maybe True (== char) $ vec Vector.!? (i + k)) $ Map.toList chars
    isDisjoint k = Map.notMember (-(k + 1)) chars && Map.notMember (len - k) chars
    usedChars k =
      Multiset.fromList . Map.elems $ Map.filterWithKey (\n _ -> n >= (-k) && n < len - k) chars
    isSpellable k = mset `Multiset.isSubsetOf` (hand <> usedChars k)
  in filter (\k -> matches k && isDisjoint k && isSpellable k) [minOffset .. maxOffset]

-- | Returns the words and offsets from the given dictionary and hand which match the constraints
-- and fit within the bounds.
matchingWords
  :: Dictionary
  -> Hand
  -> Map Int Char -- ^ Character constraints, i.e. positions where characters are already set.
  -> (Int, Int) -- ^ Bounds which cap the minimum and maximum offsets.
  -> [(Text, Int)]
matchingWords (Dictionary items) hand chars bounds =
  let
    chars' = foldl' (flip Multiset.insert) hand $ Map.elems chars
    items' = filter (spellable chars') $ Vector.toList items
  in concat $ fmap (\item -> fmap (itemTxt item,) (itemOffsets hand chars bounds item)) items'
