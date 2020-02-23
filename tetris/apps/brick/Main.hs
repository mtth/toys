{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import qualified Brick as Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Brick.Widgets.Center (center, hCenter)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (LoggingT, runLoggingT)
import Data.Foldable (toList)
import Data.Geometry.YX (YX(YX))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as VTY

import Tetris

type Name = ()
type Widget = Brick.Widget Name
data Tick = Tick
data Cell = ActiveCell | FrozenCell

numRows, numCols :: Int
numRows = 24
numCols = 10

activeAttr, frozenAttr, emptyAttr :: Brick.AttrName
activeAttr = "active"
frozenAttr = "frozen"
emptyAttr = "empty"

drawCell :: Maybe Cell -> Widget
drawCell =
  let useAttr attr = Brick.withAttr attr $ Brick.str "  "
  in \case
    Just ActiveCell -> useAttr activeAttr
    Just FrozenCell -> useAttr frozenAttr
    Nothing -> useAttr emptyAttr

gameCells :: Game -> Map YX Cell
gameCells game =
  let toMap cell = Map.fromList . fmap (,cell) . toList . ($ game)
  in toMap ActiveCell activeCoords <> toMap FrozenCell frozenCoords

gameGrid :: Game -> Widget
gameGrid game =
  let
    cells = gameCells game
    toWidget y x = drawCell $ Map.lookup (YX y x) cells
    toRow y = Brick.hBox $ fmap (toWidget y) [0..(numCols - 1)]
  in Brick.vBox $ fmap toRow [0..(numRows - 1)]

draw :: Game -> [Widget]
draw game =
  let
    Score n = currentScore game
    scoreWidget =
      Brick.withBorderStyle unicode $
      borderWithLabel (Brick.str "Score") $
      Brick.hLimit 12 $
      hCenter $
      Brick.padAll 1 $
      Brick.str (show n)
    gridWidget =
      Brick.withBorderStyle unicodeBold $
      borderWithLabel (Brick.str "Tetris") $
      gameGrid game
  in [center $ scoreWidget Brick.<+> gridWidget]

charAction :: Char -> Maybe Action
charAction = \case
  'j' -> Just MoveDown
  'l' -> Just MoveRight
  'h' -> Just MoveLeft
  'f' -> Just Rotate
  _ -> Nothing

handleEvent :: Game -> Brick.BrickEvent Name Tick -> Brick.EventM Name (Brick.Next Game)
handleEvent game = go where
  go (Brick.AppEvent Tick) = run onTick >>= \case
    Left _ -> Brick.halt game
    Right game' -> Brick.continue game'
  go (Brick.VtyEvent (VTY.EvKey (VTY.KChar char) [])) = case charAction char of
    Just action -> run (onAction action) >>= Brick.continue
    Nothing -> Brick.continue game
  go (Brick.VtyEvent (VTY.EvKey VTY.KEsc [])) = Brick.halt game
  go _ = Brick.continue game

  run :: (Game -> LoggingT w IO a) -> Brick.EventM Name a
  run step =
    let discardLog _ = pure ()
    in liftIO (runLoggingT (step game) discardLog)

attrMap :: Brick.AttrMap
attrMap = Brick.attrMap VTY.defAttr
  [ (activeAttr, VTY.blue `Brick.on` VTY.blue)
  , (frozenAttr, VTY.red `Brick.on` VTY.red)
  ]

tetrisApp :: Brick.App Game Tick Name
tetrisApp = Brick.App
  { Brick.appDraw = draw
  , Brick.appChooseCursor = Brick.neverShowCursor
  , Brick.appHandleEvent = handleEvent
  , Brick.appStartEvent = pure
  , Brick.appAttrMap = const attrMap
  }

main :: IO ()
main = do
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000
  Just game <- newGame (YX (numRows - 1) (numCols - 1))
  let buildVTY = VTY.mkVty VTY.defaultConfig
  vty <- buildVTY
  void $ Brick.customMain vty buildVTY (Just chan) tetrisApp game
