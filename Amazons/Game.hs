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

checkForWinner :: (Monad m) => GameState -> m Player
checkForWinner (GameState NewTurn p b) =
  case allValidMovesForPlayer b p of
    [] -> return (otherPlayer p)
    _ -> fail "No one has won yet"
checkForWinner _ = fail "Cannot check for a winner during a players turn"

movePieceTo :: (Monad m) => GameState -> Tile -> Tile -> m GameState
movePieceTo (GameState NewTurn _ _) peice tile =
  fail "Cannot move a piece before checking if anyone has won"
movePieceTo (GameState (Fire _) _ _) piece tile =
  fail "Cannot move piece when firing"
movePieceTo gs p t = movePieceToHelper gs p t


movePieceToHelper :: (Monad m) => GameState -> Tile -> Tile -> m GameState
movePieceToHelper gs p t
  | pieceExists && moveExists = return $ updatePiece gs p t
  | not pieceExists = fail ((show p) ++ " is not a piece you can move")
  | not moveExists = fail ((show t) ++ " cannot be moved to")
  | otherwise = fail ("Move " ++ (show p) ++ " to " ++ (show t) ++ " is not a valid move")
    where
      pieces = getPlayersPieces gs
      availableTiles = validMoveTiles (board gs) p
      pieceExists = elem p pieces
      moveExists = elem t availableTiles

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
  | notBlocked = t:(pathUntilBlocked b ts)
  | otherwise = []
    where
      blackPeice = elem t bp
      whitePiece = elem t wp
      fire = elem t wp
      notBlocked = not $ blackPeice || whitePiece || fire

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
