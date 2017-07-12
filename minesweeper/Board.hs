module Board where
  import Graphics.Gloss
  import qualified Data.Map as Map

  -- Represents a single square on the grid and its state
  data CellType = Mine | Clear Int deriving (Show, Read)
  data Cell = Cell {
      revealed :: Bool
    , cellType :: CellType
  } deriving (Show)

  -- represents the whole grid and game state
  data GameState = OnGoing | Win | Loss deriving (Show)
  type Coord = (Int, Int)
  data Board = Board {
      state :: GameState
    , size :: Int
    , cells :: Map.Map Coord Cell
  } deriving (Show)

  

  render :: Board -> Float -> Float -> Picture
  render (Board state size cells) width height =
    pictures (map cellPic [(x,y) | x <- [0..size], y <- [0..size]])
      where
        cellWidth = width / (fromIntegral size)
        cellHeight = height / (fromIntegral size)
        cellPic :: Coord -> Picture
        cellPic (x,y) = translate ((fromIntegral x) * cellWidth)
                                  ((fromIntegral y) * cellHeight) $
                        color white $
                        rectangleSolid (cellWidth-1) (cellHeight-1)
