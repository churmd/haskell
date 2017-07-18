module Main where
  import Graphics.Gloss
  import Board
  import Display
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
  playGame g = do
    let b = Board.makeBoard 3 2 g
    --putStrLn $ show b
    rendered <- Display.render b
    display (InWindow "Minesweeper" (truncate getWidth, truncate getHeight) (1,1)) white rendered
