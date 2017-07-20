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


-- Start of making new board

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

-- rewrite to make f in map.update do work not case of
  incClearVals :: Board -> [Coord] -> Board
  incClearVals b [] = b
  incClearVals b@(Board st sz cells) (c : cs) =
    case Map.lookup c cells of
      Just (Cell tf (Clear n)) ->
        let f (Cell ft (Clear n)) = Just (Cell tf (Clear (n+1))) in
        incClearVals (Board st sz (Map.update f c cells)) cs
      _ -> incClearVals b cs

  makeBoard :: (RandomGen g) => Int -> Int -> g -> Board
  makeBoard size numMines gen =
    let b = blankBoard size in
    let bMines = addMines b numMines gen in
    bMines

-- End of making new board

-- Start of board interations

  revealCell :: Board -> Coord -> Board
  revealCell b@(Board st sz cells) c =
    case Map.lookup c cells of
      Just (Cell True _) -> b
      Just (Cell _ Mine) ->
        let lossBoard = Board Loss sz cells in
        revealAllMines lossBoard
      Just (Cell _ (Clear n)) ->
        let reveal (Cell _ val) = Just (Cell True val) in
        let newCells = Map.update reveal c cells in
        let state = if (hasWon newCells) then Win else OnGoing in
        Board state sz newCells

  revealAllMines :: Board -> Board
  revealAllMines (Board st sz cells) =
    let revealMine (Cell _ Mine) = Cell True Mine
        revealMine c = c in
    Board st sz (Map.map revealMine cells)

  hasWon :: Map.Map Coord Cell -> Bool
  hasWon cells =
    let f (Cell False (Clear n)) tf = False && tf
        f (Cell True Mine) tf = False && tf
        f _ tf = True && tf in
    Map.fold f True cells

--End of board interactions

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