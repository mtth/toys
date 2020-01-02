{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST (ST, runST)
import Data.Geometry.YX (YX(YX))
import qualified Data.Map.Strict as Map
import qualified Data.Multiset as Multiset
import Data.STRef (readSTRef)
import Debug.Trace (trace)
import Test.Hspec
import Test.Hspec.QuickCheck

import Bananagrams.Dictionary
import Bananagrams.Grid

dictionarySpec :: Spec
dictionarySpec = describe "Dictionary" $ do
  it "firstWords" $ do
    let d = newDictionary ["fe", "fee"]
    firstWords d (Multiset.fromList "fee") `shouldBe` ["fee", "fe"]
  it "matchingWords" $ do
    let
      d = newDictionary ["pop"]
      h = Multiset.fromList "pop"
    matchingWords d h (Map.fromList [((-2), 'b'), (0, 'p')]) (10, 10) `shouldBe` [("pop", 0)]

gridCandidates :: Int -> [Entry] -> Either Conflict [Candidate]
gridCandidates size entries = runST $ do
  grid <- newGrid size
  let
    addEntries [] = Right <$> candidates size grid
    addEntries (e:es) = setEntry e grid >>= \case
      Left conflict -> pure $ Left conflict
      Right chars -> addEntries es
  addEntries entries

gridSpec :: Spec
gridSpec = describe "Grid" $ do
  it "candidates" $ do
    let
      cands = gridCandidates 5 [Entry "foo" Horizontal 0]
      bounds = (5, 5)
    cands `shouldBe` Right
      [ Candidate 0 Vertical (Map.singleton 0 'f') bounds
      , Candidate (YX 0 1) Vertical (Map.singleton 0 'o') bounds
      , Candidate (YX 0 2) Vertical (Map.singleton 0 'o') bounds ]
  it "case 0" $ do
    let
      cands = gridCandidates 10
        [ Entry "low" Horizontal 0
        , Entry "goo" Vertical (YX (-1) 1) ]
    cands `shouldBe` Right []
  it "case 1" $ do
    let
      cands = gridCandidates 10
        [ Entry "foamier" Horizontal 0
        , Entry "fib" Vertical 0
        , Entry "pawp" Vertical (YX (-1) 2)
        , Entry "imp" Vertical (YX 0 4)
        , Entry "bop" Horizontal (YX 2 0) ]
    cands `shouldBe` Right
      [ Candidate (YX 0 5) Vertical (Map.singleton 0 'e') (10, 0)
      , Candidate (YX 0 6) Vertical (Map.singleton 0 'r') (10, 10)
      , Candidate (YX 2 4) Horizontal (Map.singleton 0 'p') (0, 8) ]

main :: IO ()
main = hspec $ do
  dictionarySpec
  gridSpec
