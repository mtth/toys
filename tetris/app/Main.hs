{-# LANGUAGE LambdaCase #-}

module Main where

import Tetris

import Data.Foldable (toList)
import qualified Data.Geometry.YX as YX
import Graphics.Gloss (Display(..), Picture, color, greyN, makeColorI, pictures, rectangleSolid, scale, text, translate)
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), SpecialKey(..), playIO)
import System.Exit (ExitCode(..), exitWith)

-- A few colors used below.
blue = makeColorI 72 133 237 255
green = makeColorI 60 186 84 255
red = makeColorI 219 50 54 255
yellow = makeColorI 244 194 13 255

-- Number of rows and columns in the board.
numRows, numCols :: Int
numRows = 24
numCols = 10

-- Size of a block in pixels.
blockSize, height, width :: Float
blockSize = 20
height = blockSize * fromIntegral numRows
width = blockSize * fromIntegral numCols

data UserInput = UserAction Action | Exit | BadInput

userInput :: Key -> UserInput
userInput = go where
  go (Char 'j') = UserAction MoveDown
  go (Char 'h') = UserAction MoveLeft
  go (Char 'l') = UserAction MoveRight
  go (Char 'f') = UserAction Rotate
  go (SpecialKey KeyEsc) = Exit
  go _ = BadInput

onEvent :: Event -> Game -> IO Game
onEvent event game = case event of
  EventKey key Down _ _ -> case userInput key of
    UserAction action -> pure $ onAction action game
    Exit -> exitWith ExitSuccess
    _ -> pure game
  _ -> pure game

onIteration :: Game -> IO Game
onIteration game = onTick game >>= \case
  Right game' -> pure game'
  Left _ -> exitWith (ExitFailure 1) -- TODO: Show score.

toSize :: Int -> Float
toSize n = fromIntegral n * blockSize

drawGame :: Game -> IO Picture
drawGame game = pure $ pictures [backPic, activePic, frozenPic, scorePic] where
  Score num = currentScore game
  scorePic =
    translate (-width * 0.5) (height * 0.52) .
    scale 0.2 0.2 . color red . text $
    "score: " ++ show num
  backPic = color (greyN 0.9) $ rectangleSolid width height
  activePic = color green $ drawCoords $ activeCoords game
  frozenPic = color blue $ drawCoords $ frozenCoords game
  drawCoords = pictures . fmap drawCoord . toList where
    drawCoord (YX.YX y x) =
      let
        rect = rectangleSolid (toSize 1) (toSize 1)
        xPos = toSize x - (width - blockSize) / 2
        yPos = toSize (numRows - 1 - y) - (height - blockSize) / 2
      in translate xPos yPos rect

main :: IO ()
main = do
  let
    padding = 125
    size = (ceiling width + padding, ceiling height + padding)
    display = InWindow "Tetris" size (0, 0)
  Just game <- newGame (YX.YX (numRows - 1) (numCols - 1))
  playIO display (greyN 0.8) 2 game drawGame onEvent (const onIteration)
