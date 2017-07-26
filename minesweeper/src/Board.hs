module Board where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import qualified Data.List as List
  import System.Random
  import Data.Time

  -- Represents a single square on the grid and its state
  data CellType = Mine | Clear Int deriving (Show, Read)
  data Cell = Cell {
      revealed :: Bool
    , cellType :: CellType
  } deriving (Show)

  -- represents the whole grid and game state
  data GameState = OnGoing | Win | Loss deriving (Show, Eq)
  type Coord = (Int, Int)
  type TimeKeep = (Bool, UTCTime, UTCTime)
  data Board = Board {
      state :: GameState
    , size :: Int
    , numMines :: Int
    , cells :: Map.Map Coord Cell
    , time :: TimeKeep
  } deriving (Show)


-- Start of making new board

  blankBoard :: Int -> Int -> UTCTime -> Board
  blankBoard size numMines time =
    let cells = (Map.fromList
                  [((x,y), Cell False (Clear 0)) |
                  x <- [0..(size-1)],
                  y <- [0..(size-1)]]) in
    Board OnGoing size numMines cells (False, time, time)

  addMines :: (RandomGen g) => Board -> g -> Board
  addMines b@(Board _ size numMines cells t) g =
     let (g1, g2) = split g in
     let xs = (randomRs (0,size-1) g1) in
     let ys = (randomRs (0,size-1) g2) in
     let coords = getUniqueCoord numMines xs ys [] in
     (placeMines b coords)

  getUniqueCoord :: Int -> [Int] -> [Int] -> [Coord] -> [Coord]
  getUniqueCoord len [] ys acc = acc
  getUniqueCoord len xs [] acc = acc
  getUniqueCoord len (x : xs) (y : ys) acc
   | List.length acc == len = acc
   | List.elem (x,y) acc = getUniqueCoord len xs ys acc
   | otherwise = getUniqueCoord len xs ys ((x,y) : acc)

  placeMines :: Board -> [Coord] -> Board
  placeMines b [] = b
  placeMines (Board st sz nm cells t) (c : cs) =
    let f (Cell tf _) = Just (Cell tf Mine) in
    let tempBoard = (Board st sz nm (Map.update f c cells) t) in
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
  incClearVals b@(Board st sz nm cells t) (c : cs) =
    let f (Cell tf (Clear n)) = Just (Cell tf (Clear (n+1)))
        f c = Just c in
    let newCells = Map.update f c cells in
    incClearVals (Board st sz nm newCells t) cs

  makeBoard :: (RandomGen g) => Int -> Int -> UTCTime -> g -> Board
  makeBoard size numMines time gen =
    let b = blankBoard size numMines time in
    let bMines = addMines b gen in
    bMines

-- End of making new board

-- Start of board interations

  cellInRange :: Board -> Coord -> Bool
  cellInRange (Board st sz nm cells t) c =
    Map.member c cells

  revealCell :: Board -> Coord -> Board
  revealCell b@(Board st sz nm cells (running, start, end)) c =
    case Map.lookup c cells of
      Just (Cell True _) -> b
      Just (Cell _ Mine) ->
        let lossBoard = Board Loss sz nm cells (False, start, end) in
        revealAllMines lossBoard
      Just (Cell _ (Clear n)) ->
        let reveal (Cell _ val) = Just (Cell True val) in
        let newCells = Map.update reveal c cells in
        let state = if (hasWon newCells) then Win else OnGoing in
        let timeKeeper = if state == Win then
                            (False, start, end)
                          else
                            (True, start, end) in
        Board state sz nm newCells timeKeeper

  revealAllMines :: Board -> Board
  revealAllMines (Board st sz nm cells t) =
    let revealMine (Cell _ Mine) = Cell True Mine
        revealMine c = c in
    Board st sz nm (Map.map revealMine cells) t

  hasWon :: Map.Map Coord Cell -> Bool
  hasWon cells =
    let f (Cell False (Clear n)) tf = False && tf
        f (Cell True Mine) tf = False && tf
        f _ tf = True && tf in
    Map.fold f True cells

--End of board interactions
