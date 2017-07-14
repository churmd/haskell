module Main where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import Board
  import System.Random
  {--
  main :: IO ()
  main = do
    playIO
      --window
      (InWindow "Tic-tac-toe" (500, 500) (1,1))
      --background colour
      azure
      --fps
      10
      --initial world
      (initialBoard, X)
      --world to picture
      drawBoard
      --handle input events
      handleInput
      --step world 1 interation
      stepGame
      --}

  main :: IO()
  main = do
    g <- getStdGen
    playGame g

  playGame :: (RandomGen g) => g -> IO()
  playGame g =
    let b = Board.makeBoard 3 2 g in
    putStrLn $ show b
  --  display (InWindow "Minesweeper" (500, 500) (1,1)) black (Board.render bMines 500 500)
