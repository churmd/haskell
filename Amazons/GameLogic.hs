module GameLogic where
import GameState
import Board

turnStart :: GameState -> IO()
turnStart gs = do
  case checkForWinner gs of
    Just p -> putStrLn $ (show p) ++ " wins!"
    Nothing -> turnMove gs

turnMove :: GameState -> IO()
turnMove gs = undefined

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

fireArrow :: (Monad m) => GameState -> Tile -> m GameState
fireArrow(GameState NewTurn _ _) _ =
  fail "Cannot fire an arrow at the start of a turn"
fireArrow(GameState Move _ _) _ =
  fail "Cannot fire an arrow before moving"
fireArrow(GameState (Fire piece) pl b) t
  | elem t validTargets =
    return (GameState NewTurn (otherPlayer pl) (addFire b t))
  | otherwise =
    fail ("Cannot fire at tile " ++ (show t) ++ " with piece " ++ (show piece))
    where
      validTargets = validFireTiles b piece
