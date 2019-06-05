module GameState where
import Board
import Pathing

data Player = White | Black deriving (Show, Eq)
data Step = NewTurn | Move | Fire Tile deriving (Show, Eq)
data GameState = GameState {
  step :: Step,
  player :: Player,
  board :: Board
} deriving (Show)

updatePiece :: GameState -> Tile -> Tile -> GameState
updatePiece gs@(GameState _ White b) p t = gs {board = moveWhitePiece b p t}
updatePiece gs@(GameState _ Black b) p t = gs {board = moveBlackPiece b p t}

allValidMovesForPlayer :: Board -> Player -> [Tile]
allValidMovesForPlayer b White = allValidMoves b (whitePieces b)
allValidMovesForPlayer b Black = allValidMoves b (blackPieces b)

allValidMoves :: Board -> [Tile] -> [Tile]
allValidMoves b ts = concat $ map (\t -> validMoveTiles b t) ts

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
  | emptySpace b t = t:(pathUntilBlocked b ts)
  | otherwise = []

allPathsFromATile :: Board -> Tile -> [[Tile]]
allPathsFromATile b@(Board w h _ _ _) t = map (\f -> f t w h) allPathFunctions

getPlayersPieces :: GameState -> [Tile]
getPlayersPieces (GameState _ White b) = whitePieces b
getPlayersPieces (GameState _ Black b) = blackPieces b

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

basicGame :: GameState
basicGame = GameState NewTurn White b
  where
    b = Board 4 4 [(0,0), (0,3)] [(3,0), (3,3)] []
