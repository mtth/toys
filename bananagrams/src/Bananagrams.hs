{-# LANGUAGE LambdaCase #-}

-- | A simple Bananagrams solver.
--
-- This module implements a heuristic for finding a valid Bananagrams layout given a list of
-- characters and valid words. Note that for simplicity the heuristic only explores layouts where
-- words in the same direction are not adjacent, and therefore might fail to find a solution even if
-- one exists (this is however unlikely in realistic examples).
module Bananagrams (
  solve,
  Dictionary, newDictionary,
  Entry(..), Orientation(..), displayEntries
) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Data.STRef (STRef, newSTRef, modifySTRef', readSTRef)
import Data.Text (Text)
import qualified Data.Text as T

import Bananagrams.Dictionary
import Bananagrams.Grid

firstJust :: (Alternative g, Foldable f, Monad m) => (a -> m (g b)) -> f a -> m (g b)
firstJust f = foldlM (\g a -> (g <|>) <$> f a) empty

data Bananagrams s
  = Bananagrams
    { dict :: Dictionary
    , handRef :: STRef s Hand
    , grid :: Grid s }

newBananagrams :: Dictionary -> Multiset Char -> ST s (Bananagrams s)
newBananagrams dict hand = Bananagrams dict <$> newSTRef hand <*> newGrid (Multiset.size hand)

entryChars :: Entry -> Multiset Char
entryChars = Multiset.fromList . T.unpack . entryText

try :: Bananagrams s -> Entry -> ST s ()
try (Bananagrams _ ref grid) entry = do
  setEntry entry grid
  modifySTRef' ref (flip Multiset.difference $ entryChars entry)

backtrack :: Bananagrams s -> ST s ()
backtrack (Bananagrams _ ref grid) = unsetLastEntry grid >>= \case
  Nothing -> error "backtracking too far"
  Just entry -> modifySTRef' ref (<> entryChars entry)

startSolve :: Bananagrams s -> ST s (Maybe [Entry])
startSolve b@(Bananagrams d ref _) = firstWords d <$> readSTRef ref >>= firstJust tryWord where
  tryWord word = do
    try b $ Entry word Horizontal 0
    continueSolve b >>= \case
      Just entries -> pure $ Just entries
      Nothing -> backtrack b >> pure Nothing

continueSolve :: Bananagrams s -> ST s (Maybe [Entry])
continueSolve b@(Bananagrams dict ref grid) = do
  hand <- readSTRef ref
  if Multiset.null hand
    then Just <$> gridEntries grid
    else do
      cands <- candidates grid
      let
        tryCandidate (Candidate yx orient spots) =
          firstJust (tryWord yx orient) (matchingWords dict hand spots)
        tryWord yx orient (word, off) = do
          let entry = Entry word orient (yx - fromIntegral off * orientationYX orient)
          try b entry >> continueSolve b >>= maybe (backtrack b >> pure Nothing) (pure . Just)
      firstJust tryCandidate cands

solve :: Dictionary -> Multiset Char -> Maybe [Entry]
solve dict hand = runST $ newBananagrams dict hand >>= startSolve
