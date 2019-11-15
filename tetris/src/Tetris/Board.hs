module Tetris.Board (
  Piece, randomPiece, movePiece, rotatePiece, pieceCoords,
  Board, newBoard, boardCoords, boardWidth,
  fitsOn, freezePiece
) where

import Data.Foldable (toList)
import Data.Geometry.YX (YX(..))
import qualified Data.Geometry.YX as YX
import qualified Data.Map.Strict as Map
import Data.Multimap (SetMultimap)
import qualified Data.Multimap as Mmap
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- | A rectangular Tetris board.
data Board = Board
  { _boardBox :: YX.Box
  , _boardCoords :: Set YX
  } deriving (Eq, Show)

-- | Returns the coordinates of all populated positions inside the board.
boardCoords :: Board -> Set YX
boardCoords = _boardCoords

-- | Returns the number of columns in the board.
boardWidth :: Board -> Int
boardWidth = YX.boxWidth . _boardBox

-- | Creates a new board with the given position as extreme point. If at least one of the given
-- position's fields is negative, the function will return 'Nothing'.
newBoard :: YX -> Maybe Board
newBoard extent = case YX.box 0 extent of
  Just box -> Just $ Board box Set.empty
  _ -> Nothing

data Tetromino
  = I | J | L | O | S | T | Z
  deriving (Bounded, Enum, Eq, Show)

type Angle = Int

-- | A Tetris piece.
--
-- Pieces are generated via 'randomPiece'.
data Piece = Piece Angle YX Tetromino deriving (Eq, Show)

-- | Generates a random piece in the given column.
randomPiece :: Int -> IO Piece
randomPiece x = do
  a <- randomRIO (0, 3)
  t <- toEnum <$> randomRIO (0, 5)
  pure $ Piece a (YX 0 x) t

-- | Moves a piece by the given 2D offset.
movePiece :: YX -> Piece -> Piece
movePiece p0 (Piece a p1 t) = Piece a (p0 + p1) t

-- | Rotates a piece clockwise 90 degrees..
rotatePiece :: Piece -> Piece
rotatePiece (Piece a p t) = Piece ((a + 1) `mod` 4) p t

-- | Adds the piece to the board, clearing any newly filled rows. This function returns the number
-- of cleared rows along with the updated board.
freezePiece :: Piece -> Board -> (Int, Board)
freezePiece piece (Board box oldCoords) = (numLines, Board box newCoords) where
  allCoords = oldCoords <> pieceCoords piece
  rows = Mmap.fromList . fmap (\(YX y x) -> (y, x)) . toList $ allCoords
  nonFullRows = Mmap.toMap $ Mmap.filterGroups (\(_, s) -> Set.size s <= YX.boxWidth box) rows
  numLines = Mmap.distinctSize rows - Map.size nonFullRows
  height = YX.boxHeight box
  heights = [height, (height - 1)..]
  newCoords = mconcat $ zipWith toCoords (Map.toDescList nonFullRows) heights
  toCoords (_, xs) y = Set.map (YX y) xs

-- | Checks whether the given piece fits on the board (i.e. within the boundaries of the board and
-- doesn't overlap with any existing pieces).
fitsOn :: Piece -> Board -> Bool
fitsOn piece board =
  let
    box = _boardBox board
    coords = pieceCoords piece
  in all (\coord -> coord `YX.inBox` box) coords && Set.disjoint coords (_boardCoords board)

-- | Returns the piece's coordinates.
pieceCoords :: Piece -> Set YX
pieceCoords = Set.fromList . go where
  go (Piece a p I) =
    let
      coords = scanl (+) p $ replicate 4 YX.down
      rotate = YX.rotate YX.Clockwise (YX.Around (p + YX.down))
    in if a `mod` 2 == 0
      then coords
      else fmap rotate coords
  go (Piece a p J) =
    let
      coords = fmap (+ p) [0, YX.down, 2 * YX.down, 2 * YX.down + YX.left]
      rotate = YX.rotate YX.Clockwise (YX.AroundTopLeftCorner (p + 2 * YX.down))
    in iterate (fmap rotate) coords !! a
  go (Piece a p L) = fmap (YX.mirror (YX.AtColumn (YX.x p))) $ go (Piece a p J)
  go (Piece _ p O) = fmap (+ p) [0, YX.right, YX.down, YX.down + YX.right]
  go (Piece a p S) =
    let
      coords = fmap (+ p) [0, YX.right, YX.down, YX.down + YX.left]
      rotate = YX.rotate YX.Clockwise (YX.AroundTopLeftCorner (p + YX.down))
    in if a `mod` 2 == 0
      then coords
      else fmap rotate coords
  go (Piece a p T) =
    let
      coords = fmap (+ p) [0, YX.down + YX.left, YX.down, YX.down + YX.right]
      rotate = YX.rotate YX.Clockwise (YX.Around (p + YX.down))
    in iterate (fmap rotate) coords !! a
  go (Piece a p Z) = fmap (YX.mirror (YX.AtColumn (YX.x p))) $ go (Piece a p J)
