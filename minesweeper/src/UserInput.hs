module UserInput where
  import Graphics.Gloss.Interface.Pure.Game
  import Board
  import Globals
  import System.Random
  import Data.Time

  handler :: Event -> Board -> IO (Board)
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b@(Board Win _ _ _ _) =
    return b
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b@(Board Loss _ _ _ _) =
    return b
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b = do
    let c = getBoardCoord (x,y) (getWidth) (getHeight) b
    if cellInRange b c then
      let started = startGame b in
      return (revealCell started c)
    else
      return b
  handler (EventKey (Char 'r') Up _ _) (Board st sz nm t cells) = do
    g <- newStdGen
    time <- getCurrentTime
    return (makeBoard sz nm time g)
  handler (EventKey (Char 'e') Up _ _) b = do
    g <- newStdGen
    time <- getCurrentTime
    return (makeBoard easyBoard easyMines time g)
  handler (EventKey (Char 'm') Up _ _) b = do
    g <- newStdGen
    time <- getCurrentTime
    return (makeBoard medBoard medMines time g)
  handler (EventKey (Char 'h') Up _ _) b = do
    g <- newStdGen
    time <- getCurrentTime
    return (makeBoard hardBoard hardMines time g)
  handler _ b = return b

  getBoardCoord :: (Float, Float) -> Float -> Float -> Board -> Coord
  getBoardCoord (x,y) width height (Board st sz nm t cells) =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let yOffset = (height/2) in
    let xOffset = (width/2) in
    let adjustedX = x + xOffset in
    let adjustedY = y + yOffset in
    (floor (adjustedX/cellWidth), floor (adjustedY/cellHeight))
