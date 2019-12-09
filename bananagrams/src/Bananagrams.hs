-- | A simple Bananagrams solver.
--
-- This module implements a heuristic for finding a valid Bananagrams layout given a list of
-- characters and valid words. Note that for simplicity the heuristic only explores layouts where
-- words in the same direction are not adjacent, and therefore might fail to find a solution even if
-- one exists (this is however unlikely in realistic examples).
module Bananagrams (
  Hand, newHand
) where

import Data.Map.Strict (Map)
import Data.Multiset (Multiset)
import Data.Text (Text)

import Bananagrams.Dictionary
import Bananagrams.Grid

data Hand
  = Hand
    { handChars :: !(Multiset Char)
    , handWords :: !(Map Text Double) }

-- | Generates a new 'Hand' for the given path and characters.
newHand :: [Text] -> Multiset Char -> Hand
newHand = undefined
