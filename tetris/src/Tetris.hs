{-# LANGUAGE LambdaCase #-}

-- | A simple Tetris implementation.
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

-- | A Tetris game's state.
data Game = Game
  { _gameScore :: Int
  , _gameBoard :: Board
  , _gamePiece :: Piece
  }

-- | Generates a new gave with the given extent. For example, a game with height 24 and width 10
-- (the standard) can be generated with @newGame (YX 24 10)@.
newGame :: YX -> IO (Maybe Game)
newGame extent = case newBoard extent of
  Just board -> do
    piece <- randomPiece (boardWidth board `div` 2)
    pure $ if piece `fitsOn` board
      then Just $ Game 0 board piece
      else Nothing
  _ -> pure Nothing

-- | A thin wrapper around a game's score.
newtype Score = Score Int

-- | Returns the number of full rows cleared so far.
currentScore :: Game -> Score
currentScore = Score . _gameScore

-- | Returns the active piece's coordinates.
activeCoords :: Game -> Set YX
activeCoords = pieceCoords . _gamePiece

-- | Returns the coordinates of all inactive pieces.
frozenCoords :: Game -> Set YX
frozenCoords = boardCoords . _gameBoard

-- | A user action.
data Action
 = MoveDown
 | MoveLeft
 | MoveRight
 | Rotate

-- | Updates the state of a game with a user's action. If the user's action is invalid (e.g. moving
-- a piece out of bounds), the game's state will remain unchanged.
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

-- | Updates the state of the game with the passage of time. More precisely, the following actions
-- are performed in order:
--
-- * Attempts to move the active piece down. If this is a valid move, the function simply returns
-- the updated game's state.
-- * Freezes the active piece.
-- * Clears all newly full rows, increasing the score and shifting other rows correspondingly.
-- * Generates a new random piece, if this is not possible the game is over and the function returns
-- the game's final score instead of the updated state.
onTick :: Game -> IO (Either Score Game)
onTick game = do
  let
    oldPiece = _gamePiece game
    oldBoard = _gameBoard game
    movedPiece = movePiece YX.down oldPiece
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
