{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.ST (runST)
import Data.Geometry.YX (YX(YX))
import qualified Data.Map.Strict as Map
import qualified Data.Multiset as Multiset
import Test.Hspec
import Test.Hspec.QuickCheck

import Bananagrams.Dictionary
import Bananagrams.Grid

dictionarySpec :: Spec
dictionarySpec = describe "Dictionary" $ do
  it "firstWords" $ do
    let d = newDictionary ["fe", "fee"]
    firstWords d (Multiset.fromList "fee") `shouldBe` ["fee", "fe"]

gridSpec :: Spec
gridSpec = describe "Grid" $ do
  it "candidates" $ do
    let
      cands = runST $ do
        grid <- newGrid 5
        setEntry (Entry "foo" Horizontal 0) grid
        candidates grid
    cands `shouldBe`
      [ Candidate 0 Vertical (Map.singleton 0 'f')
      , Candidate (YX 0 1) Vertical (Map.singleton 0 'o')
      , Candidate (YX 0 2) Vertical (Map.singleton 0 'o') ]

main :: IO ()
main = hspec $ do
  dictionarySpec
  gridSpec
