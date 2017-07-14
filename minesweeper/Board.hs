module Board where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import qualified Data.List as List
  import System.Random

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

  blankBoard :: Int -> Board
  blankBoard size = Board OnGoing size (Map.fromList
                [((x,y), Cell False (Clear 0)) | x <- [0..(size-1)], y <- [0..(size-1)]])

  addMines :: (RandomGen g) => Board -> Int -> g -> Board
  addMines b@(Board _ size cells) numMines g =
     let (g1, g2) = split g in
     let xs = take numMines $ List.nub $ randomRs (0,size-1) g1 in
     let ys = take numMines $ List.nub $ randomRs (0,size-1) g2 in
     let coords = zip xs ys in
     (placeMines b coords)
     

  placeMines :: Board -> [Coord] -> Board
  placeMines b [] = b
  placeMines (Board st sz cells) (c : cs) =
    let f (Cell tf _) = Just (Cell tf Mine) in
    let tempBoard = (Board st sz (Map.update f c cells)) in
    let newBoard = incClearVals tempBoard (surroundingCoords c sz) in
    placeMines newBoard cs

  surroundingCoords :: Coord -> Int -> [Coord]
  surroundingCoords (x, y) size =
    let allPoss = [(x-1,y),(x-1,y-1),(x-1,y+1),
                   (x,y-1),(x,y+1),
                   (x+1,y),(x+1,y-1),(x+1,y+1)] in
    let test (x,y) = x >= 0 && x < size && y >= 0 && y < size in
    filter test allPoss

  incClearVals :: Board -> [Coord] -> Board
  incClearVals b [] = b
  incClearVals b@(Board st sz cells) (c : cs) =
    case Map.lookup c cells of
      Just (Cell tf (Clear n)) ->
        let f (Cell ft (Clear n)) = Just (Cell tf (Clear (n+1))) in
        incClearVals (Board st sz (Map.update f c cells)) cs
      _ -> incClearVals b cs

  makeBoard :: Int -> Int -> Board
  makeBoard size numMines = undefined


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
