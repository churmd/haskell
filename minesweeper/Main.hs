module Main where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import Board
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
  main = display (InWindow "Minesweeper" (500, 500) (1,1)) black (Board.render (Board OnGoing 5 (Map.fromList [])) 500 500 )
