{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple Bananagrams solver.
--
-- This module implements a heuristic for finding a valid Bananagrams layout given a list of
-- characters and valid words. Note that for simplicity the heuristic only explores layouts where
-- words in the same direction are not adjacent, and therefore might fail to find a solution even if
-- one exists (this is however unlikely in realistic examples).
module Bananagrams (
  -- * Generate inputs
  Dictionary, newDictionary, Hand,
  -- * Find a solution
  solve, Severity(..),
  -- * Inspect the solution
  Entry(..), Orientation(..), displayEntries
) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad.Log (DiscardLoggingT(discardLogging))
import Control.Monad.ST (RealWorld)
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import Data.Multiset (Multiset)
import qualified Data.Multiset as Multiset
import Data.STRef (STRef, newSTRef, modifySTRef', readSTRef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)

import Bananagrams.Dictionary
import Bananagrams.Grid
import Bananagrams.Log

firstJust :: (Foldable f, Monad m) => (a -> m (Maybe b)) -> f a -> m (Maybe b)
firstJust f = foldlM (\mb a -> case mb of { Just _ -> pure mb ; Nothing -> f a }) empty

data Bananagrams
  = Bananagrams
    { dict :: Dictionary
    , handRef :: STRef RealWorld Hand
    , grid :: Grid RealWorld }

newBananagrams :: LoggableIO m => Dictionary -> Multiset Char -> m Bananagrams
newBananagrams dict hand = liftST $ Bananagrams dict <$> newSTRef hand <*> newGrid (Multiset.size hand)

entryChars :: Entry -> Multiset Char
entryChars = Multiset.fromList . T.unpack . entryText

try :: LoggableIO m => Bananagrams -> Entry -> m ()
try (Bananagrams _ handRef grid) entry = do
  log1 Debug "Trying {}" (Shown entry)
  liftST (setEntry entry grid) >>= \case
    Left conflict -> do
      bs <- liftST (displayGrid grid)
      log1 Error "Conflict:\n{}" (decodeLatin1 bs)
      error $ "conflict: " ++ show conflict
    Right chars -> liftST $ modifySTRef' handRef (flip Multiset.difference chars)

backtrack :: LoggableIO m => Bananagrams -> m ()
backtrack (Bananagrams _ ref grid) = do
  chars <- liftST (unsetLastEntry grid)
  liftST $ modifySTRef' ref (<> chars)
  log1 Debug "Backtracked, removing {}" (Shown chars)

startSolve :: LoggableIO m => Bananagrams -> m (Maybe [Entry])
startSolve b@(Bananagrams d ref _) = do
  log0 Informational "Starting solve"
  hand <- liftST (readSTRef ref)
  let
    tryWord word = do
      log1 Informational "Selecting first word {}" word
      try b $ Entry word Horizontal 0
      continueSolve b >>= \case
        Just entries -> pure $ Just entries
        Nothing -> backtrack b >> pure Nothing
  firstJust tryWord $ firstWords d hand

continueSolve :: LoggableIO m => Bananagrams -> m (Maybe [Entry])
continueSolve b@(Bananagrams dict ref grid) = do
  hand <- liftST $ readSTRef ref
  if Multiset.null hand
    then do
      log0 Informational "Completed"
      Just <$> liftST (currentEntries grid)
    else do
      log1 Debug "Remaining letters: {}" (Shown hand)
      cands <- liftST $ candidates (Multiset.size hand + 2) grid -- TODO: Find better estimate.
      log1 Debug "Found {} candidates" (length cands) -- TODO: Remove.
      let
        tryCandidate cand@(Candidate yx orient chars bounds) = do
          log1 Debug "Trying {}" (Shown cand)
          firstJust (tryWord yx orient) (matchingWords dict hand chars bounds)
        tryWord yx orient (word, off) = do
          let entry = Entry word orient (yx - fromIntegral off * orientationYX orient)
          try b entry >> continueSolve b >>= maybe (backtrack b >> pure Nothing) (pure . Just)
      firstJust tryCandidate cands

-- | Solves a game, assembling the characters into valid words as a grid.
solve
  :: Severity -- ^ The minimum severity for which to log messages (to @stderr@).
  -> Dictionary -- ^ The dictionary of allowed words.
  -> Hand -- ^ The characters to assemble within the grid.
  -> IO (Maybe [Entry])
solve sev dict hand = loggingToStderr sev $ newBananagrams dict hand >>= startSolve
