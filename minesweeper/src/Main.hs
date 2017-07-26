module Main where
  import Graphics.Gloss.Interface.IO.Game
  import Board
  import Display
  import Globals
  import StepWorld
  import UserInput
  import System.Random
  import Data.Time

  main :: IO ()
  main = do
    g <- newStdGen
    time <- getCurrentTime
    playIO
      --window
      (InWindow "Minesweeper" (truncate getScreenWidth, truncate getScreenHeight) (1,1))
      --background colour
      white
      --fps
      10
      --initial world
      (makeBoard easyBoard easyMines time g)
      --world to picture
      Display.render
      --handle input events
      handler
      --step world 1 interation
     step

{--
  main :: IO()
  main = do
    g <- getStdGen
    playGame g

  playGame :: (RandomGen g) => g -> IO()
  playGame g = do
    let b = Board.makeBoard 4 2 g
    --putStrLn $ show b
    rendered <- Display.render b
    display (InWindow "Minesweeper" (truncate getWidth, truncate getHeight) (1,1)) white rendered
--}
