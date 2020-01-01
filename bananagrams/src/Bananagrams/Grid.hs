{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bananagrams grid operations
module Bananagrams.Grid (
  -- * Construction
  Grid, newGrid,
  -- * Accessors
  Entry(..), Orientation(..), orientationYX, gridEntries,
  -- * Modification
  -- ** Primitives
  Conflict(..), setEntry, unsetLastEntry,
  -- ** Candidate locations
  Candidate(..), candidates,
  -- * Debugging
  displayGrid, displayEntries
) where

import Algebra.Lattice (joins1, meets1)
import Control.Applicative ((<|>))
import Control.Monad.ST (ST, runST)
import qualified Data.Array.IArray as IArray
import qualified Data.Array.MArray as MArray
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unsafe as Array
import Data.Bits ((.&.), clearBit, popCount, setBit, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (chr, ord)
import Data.Foldable (foldl', for_, toList)
import Data.Geometry.YX (YX(..))
import qualified Data.Geometry.YX as YX
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Multiset (Multiset)
import qualified Data.Multiset as Mset
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)

-- | An entry's orientation.
data Orientation = Horizontal | Vertical deriving (Eq, Ord, Enum, Bounded, Show)

orientationYX :: Orientation -> YX
orientationYX Horizontal = YX.right
orientationYX Vertical = YX.down

otherOrientation :: Orientation -> Orientation
otherOrientation Horizontal = Vertical
otherOrientation Vertical = Horizontal

data Direction = U | L | D | R deriving (Eq, Ord, Enum, Bounded, Show)

directionYX :: Direction -> YX
directionYX U = YX.up
directionYX L = YX.left
directionYX D = YX.down
directionYX R = YX.right

directions :: [Direction]
directions = [U, L, D, R]

-- | A type alias for values stored in a grid. This is useful to be able to store them in an
-- efficient unboxed array.
type Letter = Word16

unknownLetter :: Letter
unknownLetter = 0

letterChar :: Letter -> Char
letterChar letter = if letterCount letter == 0
  then ' '
  else chr $ fromIntegral $ letter .&. 255

letterCount :: Letter -> Int
letterCount letter = fromIntegral $ shiftR letter 8 .&. 15

-- | Returns a letter updated to contain the character. If the letter was already populated (i.e.
-- had a positive 'letterCount') with a different character or if called more than 31 times,
-- 'setChar' will return 'Nothing'.
setChar :: Char -> Letter -> Maybe Letter
setChar char letter =
  let
    char' = letterChar letter
    letter' = (letter .&. 65280) + 256 + fromIntegral (ord char)
  in if char' == ' ' || char' == char
    then Just letter'
    else Nothing

unsetChar :: Letter -> Letter
unsetChar letter = letter - 256

neighborCount :: Letter -> Int
neighborCount letter = fromIntegral $ shiftR letter 12

addNeighbor :: Letter -> Letter
addNeighbor letter = letter + 4096

delNeighbor :: Letter -> Letter
delNeighbor letter = letter - 4096

neighbors :: YX -> [YX]
neighbors yx =
  fmap (+ yx) [YX.up + YX.left, YX.left + YX.down, YX.down + YX.right, YX.right + YX.up]

-- | A word entry inside a grid.
data Entry
  = Entry
    { entryText :: !Text
    , entryOrientation :: !Orientation
    , entryStart :: !YX
    } deriving (Eq, Ord, Show)

entryEnd :: Entry -> YX
entryEnd (Entry txt orient start) = start + fromIntegral (T.length txt - 1) * orientationYX orient

entryYXs :: Entry -> [YX]
entryYXs entry = MArray.range (entryStart entry, entryEnd entry)

-- | A Bananagrams grid!
data Grid s
  = Grid
    { _gridEntries :: !(STRef s (Seq Entry))
    , _gridArray :: !(STUArray s YX Letter) }

gridEntries :: Grid s -> ST s [Entry]
gridEntries (Grid ref _) = toList <$> readSTRef ref

-- | Generates a new grid of edge length @2*n+1@, centered around 0.
newGrid :: Int -> ST s (Grid s)
newGrid size =
  let yx = YX (size + 2) (size + 2) -- Pad to simplify neighbor handling.
  in Grid <$> newSTRef Seq.empty <*> MArray.newArray (-yx, yx) unknownLetter

modifyArray :: MArray.MArray a e m => a YX e -> YX -> (e -> e) -> m ()
modifyArray arr yx f = do
  v <- MArray.readArray arr yx
  MArray.writeArray arr yx (f v)

-- | A conflict when adding an entry.
data Conflict
  = Conflict
    { conflictYX :: !YX
    , conflictOldChar :: Char
    , conflictNewChar :: Char
    } deriving Show

-- | Attempts to add a new entry to the grid. Note that this method does *not* check that a word is
-- valid. If the word is too long, this method will 'error'.
setEntry :: Entry -> Grid s -> ST s (Maybe Conflict)
setEntry entry@(Entry txt orient start) (Grid entriesRef arr) = do
  let
    dyx = orientationYX orient
    setChars chars yx = case chars of
      (char : chars') -> do
        letter <- MArray.readArray arr yx
        case setChar char letter of
          Nothing -> pure . Just $ Conflict yx (letterChar letter) char
          Just letter' -> do
            MArray.writeArray arr yx letter'
            for_ (neighbors yx) $ \yx' -> modifyArray arr yx' addNeighbor
            setChars chars' (yx + dyx)
      [] -> do
        modifySTRef' entriesRef (Seq.|> entry)
        pure Nothing
  setChars (T.unpack txt) start

-- | Removes the last added entry, or does nothing if the grid doesn't contain any entries.
unsetLastEntry :: Grid s -> ST s (Maybe Entry)
unsetLastEntry (Grid entriesRef arr) = readSTRef entriesRef >>= \case
  Seq.Empty -> pure Nothing
  entries' Seq.:|> entry@(Entry txt orient start) -> do
    let
      dyx = orientationYX orient
      unsetChars n yx = if n == 0
        then pure ()
        else do
          modifyArray arr yx unsetChar
          for_ (neighbors yx) $ \yx' -> modifyArray arr yx' delNeighbor
          unsetChars (n - 1) (yx + dyx)
    writeSTRef entriesRef entries'
    unsetChars (T.length txt) start
    pure $ Just entry

data GridValue
  = GridValue
    { gridValueYX :: !YX
    , gridValueChar :: !Char
    , gridValueCount :: !Int
    , gridValueNeighborCount :: !Int
    , gridValueDirection :: !Orientation }

-- | Returns all set values inside the grid (i.e. with positive count). Neighbors are not returned.
gridValues :: Grid s -> ST s [GridValue]
gridValues (Grid ref arr) = do
  entries <- readSTRef ref
  let
    toValue orient yx = do
      letter <- MArray.readArray arr yx
      pure $ GridValue yx (letterChar letter) (letterCount letter) (neighborCount letter) orient
    toTuples entry = traverse (toValue $ entryOrientation entry) $ entryYXs entry
  concat <$> traverse toTuples entries

-- | Returns the smallest box containing all input entries, or nothing if no entries were given.
entriesBox :: Seq Entry -> Maybe YX.Box
entriesBox entries = case entries of
  Seq.Empty -> Nothing
  entry Seq.:<| entries' ->
    let entries'' = entry :| toList entries'
    in YX.box (meets1 $ fmap entryStart entries'') (joins1 $ fmap entryEnd entries'')

-- | A candidate grid location.
data Candidate
  = Candidate
    { candidateYX :: !YX
    -- ^ The candidate root coordinate, always already set in the grid.
    , candidateOrientation :: !Orientation
    -- ^ The orientation of the new word.
    , candidateChars :: !(Map Int Char)
    -- ^ All set characters in this candidate row or column, indexed from the root.
    } deriving (Eq, Ord, Show)

-- | Returns all candidate locations for adding new words to the grid. Note that word expansions are
-- not considered valid candidates (e.g. @T@ transforming @CAT@ into @CATS@ would not be returned).
candidates :: Grid s -> ST s [Candidate]
candidates grid = catMaybes . fmap toCand <$> gridValues grid where
  toCand (GridValue yx char 1 0 orient) =
    Just $ Candidate yx (otherOrientation orient) (Map.singleton 0 char) -- TODO: Add other chars.
  toCand _ = Nothing

displayGrid :: Grid s -> ST s ByteString
displayGrid (Grid entries arr) = fmap entriesBox (readSTRef entries) >>= \case
  Nothing -> pure ""
  Just box -> do
    arr' <- Array.unsafeFreeze arr
    let subArr = IArray.ixmap (YX.topLeft box, YX.bottomRight box) id arr' :: UArray YX Letter
    pure $ YX.arrayToByteString letterChar subArr

-- | Returns a human-readable representation of the entries.
displayEntries :: [Entry] -> Either Conflict ByteString
displayEntries entries =
  let yxs = concat $ fmap (\e -> [entryStart e, entryEnd e]) entries
  in case concat $ fmap (\(YX y x) -> [abs x, abs y]) yxs of
    [] -> Right ""
    vs -> runST $ do
        grid <- newGrid $ maximum vs
        conflicts <- catMaybes <$> traverse (\e -> setEntry e grid) entries
        case conflicts of
          [] -> Right <$> displayGrid grid
          conflict : _ -> pure $ Left conflict
