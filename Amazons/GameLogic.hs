module GameLogic where
import GameState
import Board
import System.IO
import System.IO.Error
import Control.Exception

data Result = Result GameState String deriving (Show)

playGame :: GameState -> IO()
playGame gs@(GameState NewTurn _ _) = do
  result <- turnWinnerCheck gs
  endOnMessage result
playGame gs@(GameState Move _ _) = do
  result <- turnMove gs
  continueOnMessage result
playGame gs@(GameState (Fire _) _ _) = do
  result <- turnFire gs
  continueOnMessage result

continueOnMessage :: Result -> IO()
continueOnMessage (Result gs "") = playGame gs
continueOnMessage (Result gs msg) = do
  putStrLn msg
  playGame gs

endOnMessage :: Result -> IO()
endOnMessage (Result gs "") = playGame gs
endOnMessage (Result gs msg) = putStrLn msg

turnWinnerCheck :: GameState -> IO (Result)
turnWinnerCheck gs@(GameState NewTurn _ _) = do
  winner <- checkForWinner gs
  case winner of
    Just p -> return (Result gs ((show p) ++ " wins!"))
    Nothing -> return (Result (gs { step = Move }) "")

turnMove :: GameState -> IO (Result)
turnMove gs@(GameState Move p b) = do
  putStrLn $ (show p) ++ "\'s turn"
  putStrLn $ show b
  putStrLn "Pick a piece and a tile to move it to"
  putStrLn "Enter the X value of the piece to move"
  pxString <- getLine
  putStrLn "Enter the Y value of the piece to move"
  pyString <- getLine
  putStrLn "Enter the X value of the tile to move to"
  txString <- getLine
  putStrLn "Enter the Y value of the tile to move to"
  tyString <- getLine
  let px = read pxString :: Int
  let py = read pyString :: Int
  let tx = read txString :: Int
  let ty = read tyString :: Int
  result <- movePieceTo gs (px,py) (tx,ty)
  return result
turnMove _ = fail "Cannot move a move a piece at this stage"

turnFire :: GameState -> IO (Result)
turnFire gs@(GameState (Fire piece) p b) = do
  putStrLn $ show b
  putStrLn $ "Choose a tile for piece " ++ (show piece) ++ " to fire an arrow at"
  putStrLn "Enter the X value of the tile to fire at"
  fxString <- getLine
  putStrLn "Enter the Y value of the tile to fire at"
  fyString <- getLine
  let fx = read fxString :: Int
  let fy = read fyString :: Int
  result <- fireArrow gs (fx, fy)
  return result
turnFire _ = fail "Cannot fire an arrow at this stage"


checkForWinner :: (Monad m) => GameState -> m (Maybe Player)
checkForWinner (GameState NewTurn p b) =
  case allValidMovesForPlayer b p of
    [] -> return $ Just (otherPlayer p)
    _ -> return Nothing
checkForWinner _ = fail "Cannot check for a winner during a players turn"

movePieceTo :: (Monad m) => GameState -> Tile -> Tile -> m Result
movePieceTo (GameState NewTurn _ _) peice tile =
  fail "Cannot move a piece before checking if anyone has won"
movePieceTo (GameState (Fire _) _ _) piece tile =
  fail "Cannot move piece when firing"
movePieceTo gs p t =
  case movePieceToHelper gs p t of
    Right newGS -> return (Result (newGS { step = Fire t }) "")
    Left msg -> return (Result gs msg)


movePieceToHelper :: GameState -> Tile -> Tile -> Either String GameState
movePieceToHelper gs p t
  | pieceExists && moveExists = Right (updatePiece gs p t)
  | not pieceExists = Left ((show p) ++ " is not a piece you can move")
  | not moveExists = Left ((show t) ++ " cannot be moved to")
  | otherwise = Left ("Move " ++ (show p) ++ " to " ++ (show t) ++ " is not a valid move")
    where
      pieces = getPlayersPieces gs
      availableTiles = validMoveTiles (board gs) p
      pieceExists = elem p pieces
      moveExists = elem t availableTiles

fireArrow :: (Monad m) => GameState -> Tile -> m Result
fireArrow (GameState NewTurn _ _) _ =
  fail "Cannot fire an arrow at the start of a turn"
fireArrow (GameState Move _ _) _ =
  fail "Cannot fire an arrow before moving"
fireArrow gs t =
  case fireArrowHelper gs t of
    Right newGS -> return (Result (newGS { step = NewTurn }) "")
    Left msg -> return (Result gs msg)


fireArrowHelper :: GameState -> Tile -> Either String GameState
fireArrowHelper gs@(GameState (Fire piece) pl b) t
  | elem t validTargets =
    Right (GameState NewTurn (otherPlayer pl) (addFire b t))
  | otherwise =
    Left ("Cannot fire at tile " ++ (show t) ++ " with piece " ++ (show piece))
    where
      validTargets = validFireTiles b piece
