module Board where
  import Graphics.Gloss
  import qualified Data.Map as Map

  -- Represents a single square on the grid and its state
  data CellType = Mine | Clear Int deriving (Show, Read)
  data Cell = Cell {
      revealed :: Bool
    , stuff :: CellType
  } deriving (Show)

  -- represents the whole grid and game state
  data GameState = OnGoing | Win | Loss deriving (Show)
  type Coord = (Int, Int)
  data Board = Board {
      state :: GameState
    , cells :: Map.Map Coord Cell
  } deriving (Show)
