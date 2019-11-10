{-# LANGUAGE LambdaCase #-}

module Tetris (
  Game, newGame, activeCoords, frozenCoords,
  Action(..), onAction,
  Score(..), currentScore, onTick
) where

import Tetris.Board

import Data.Geometry.YX (YX)
import qualified Data.Geometry.YX as YX
import Data.Set (Set)
import qualified Data.Set as Set

data Game = Game
  { _gameScore :: Int
  , _gameBoard :: Board
  , _gamePiece :: Piece
  }

newGame :: YX -> IO (Maybe Game)
newGame extent = case newBoard extent of
  Just board -> do
    piece <- randomPiece (boardWidth board `div` 2)
    pure $ if piece `fitsOn` board
      then Just $ Game 0 board piece
      else Nothing
  _ -> pure Nothing

newtype Score = Score Int

currentScore :: Game -> Score
currentScore = Score . _gameScore

activeCoords :: Game -> Set YX
activeCoords = pieceCoords . _gamePiece

frozenCoords :: Game -> Set YX
frozenCoords = boardCoords . _gameBoard

data Action
 = MoveDown
 | MoveLeft
 | MoveRight
 | Rotate

onAction :: Action -> Game -> Game
onAction action game = tryMove (actionMove action) where
  tryMove fn =
    let
      board = _gameBoard game
      piece = fn $ _gamePiece game
    in if piece `fitsOn` board
      then game { _gamePiece = piece }
      else game
  actionMove MoveDown = movePiece YX.down
  actionMove MoveLeft = movePiece YX.left
  actionMove MoveRight = movePiece YX.right
  actionMove Rotate = rotatePiece

onTick :: Game -> IO (Either Score Game)
onTick game = do
  let
    oldPiece = _gamePiece game
    oldBoard = _gameBoard game
  let movedPiece = movePiece YX.down oldPiece
  if movedPiece `fitsOn` oldBoard
    then pure $ Right game { _gamePiece = movedPiece }
    else do
      let
        (numLines, newBoard) = freezePiece oldPiece oldBoard
        newScore = _gameScore game + numLines
      newPiece <- randomPiece (boardWidth oldBoard `div` 2)
      pure $ if newPiece `fitsOn` newBoard
        then Right game
          { _gamePiece = newPiece
          , _gameBoard = newBoard
          , _gameScore = newScore }
        else Left $ Score newScore
