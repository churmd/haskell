module Game where

type Tile = (Int, Int)
data Player = Black | White deriving (Show, Eq)
data Step = Move | Fire Tile deriving (Show, Eq)

data Board = Board {
  width :: Int,
  height :: Int,
  blackPieces :: [Tile],
  whitePieces :: [Tile],
  fires :: [Tile]
} deriving (Show)

data GameState = GameState {
  player :: Player,
  step :: Step,
  board :: Board
} deriving (Show)

data UpdateResult = Success | Fail String

type PathFunction = Tile -> Int -> Int -> [Tile]


validFireTiles :: Board -> Tile -> [Tile]
validFireTiles b@(Board w h _ _ _) piece = filter (\t -> emptySpace b t) allTiles
  where
    allTiles = concat $ map (\f -> f piece w h) allPathFunctions

allPathFunctions :: [PathFunction]
allPathFunctions =
  [upPath, downPath, leftPath, rightPath, rightUpPath,
  rightDownPath, leftUpPath, leftDownPath]

upPath :: PathFunction
upPath t w h = path t (\(x,y) -> (x,y+1)) (\(x,y) -> y==h)

downPath :: PathFunction
downPath t w h = path t (\(x,y) -> (x,y-1)) (\(x,y) -> y<0)

rightPath :: PathFunction
rightPath t w h = path t (\(x,y) -> (x+1,y)) (\(x,y) -> x==w)

leftPath :: PathFunction
leftPath t w h = path t (\(x,y) -> (x-1,y)) (\(x,y) -> x<0)

rightUpPath :: PathFunction
rightUpPath t w h = path t (\(x,y) -> (x+1,y+1)) (\(x,y) -> x==w || y==h)

rightDownPath :: PathFunction
rightDownPath t w h = path t (\(x,y) -> (x+1,y-1)) (\(x,y) -> x==w || y<0)

leftDownPath :: PathFunction
leftDownPath t w h = path t (\(x,y) -> (x-1,y-1)) (\(x,y) -> x<0 || y<0)

leftUpPath :: PathFunction
leftUpPath t w h = path t (\(x,y) -> (x-1,y+1)) (\(x,y) -> x<0 || y==h)

path :: Tile -> (Tile -> Tile) -> (Tile -> Bool) -> [Tile]
path t inc stop | not $ stop nextTile = nextTile:(path nextTile inc stop)
                | otherwise = []
                  where
                    nextTile = inc t

emptySpace :: Board -> Tile -> Bool
emptySpace b@(Board _ _ bp wp f) c = notInBP && notInWP && notInF
  where
    notInBP = notElem c bp
    notInWP = notElem c wp
    notInF = notElem c f

inBounds :: Board -> Tile -> Bool
inBounds (Board w h _ _ _) (x, y) = xGreater && yGreater && xLess && yLess
  where
    xGreater = x >= 0
    yGreater = y >= 0
    xLess = x < w
    yLess = y < h

basicGame :: GameState
basicGame = GameState White Move b
  where
    b = Board 4 4 [(0,0), (0,3)] [(3,0), (3,3)] []
