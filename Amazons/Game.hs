module Game where
import Board
import Pathing

data GameState = GameState {
  player :: Player,
  step :: Step,
  board :: Board
} deriving (Show)

data UpdateResult = Success | Fail String

validMoveTiles :: Board -> Tile -> [Tile]
validMoveTiles b piece = concat $ allTrimmedPaths
  where
    allFullPaths = allPathsFromATile b piece
    allTrimmedPaths = map (\ts -> pathUntilBlocked b ts) allFullPaths

validFireTiles :: Board -> Tile -> [Tile]
validFireTiles b piece = filter (\t -> emptySpace b t) allTiles
  where
    allTiles = concat $ allPathsFromATile b piece

pathUntilBlocked :: Board -> [Tile] -> [Tile]
pathUntilBlocked _ [] = []
pathUntilBlocked b@(Board _ _ bp wp f) (t:ts)
  | notBlocked = t:(pathUntilBlocked b ts)
  | otherwise = []
    where
      blackPeice = elem t bp
      whitePiece = elem t wp
      fire = elem t wp
      notBlocked = not $ blackPeice || whitePiece || fire

allPathsFromATile :: Board -> Tile -> [[Tile]]
allPathsFromATile b@(Board w h _ _ _) t = map (\f -> f t w h) allPathFunctions

basicGame :: GameState
basicGame = GameState White Move b
  where
    b = Board 4 4 [(0,0), (0,3)] [(3,0), (3,3)] []
