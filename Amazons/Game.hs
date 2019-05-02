module Game where
import Board
import Pathing

data Player = White | Black deriving (Show, Eq)
data Step = NewTurn | Move | Fire Tile deriving (Show, Eq)
data GameState = GameState {
  step :: Step,
  player :: Player,
  board :: Board
} deriving (Show)


checkForWinner :: GameState -> Maybe Player
checkForWinner (GameState _ p b) =
  case allValidMovesForPlayer b p of
    [] -> Just (otherPlayer p)
    _ -> Nothing

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
  | notBlocked = t:(pathUntilBlocked b ts)
  | otherwise = []
    where
      blackPeice = elem t bp
      whitePiece = elem t wp
      fire = elem t wp
      notBlocked = not $ blackPeice || whitePiece || fire

allPathsFromATile :: Board -> Tile -> [[Tile]]
allPathsFromATile b@(Board w h _ _ _) t = map (\f -> f t w h) allPathFunctions

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

basicGame :: GameState
basicGame = GameState NewTurn White b
  where
    b = Board 4 4 [(0,0), (0,3)] [(3,0), (3,3)] []
