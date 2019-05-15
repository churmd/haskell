module GameLogic where
import GameState
import Board

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
