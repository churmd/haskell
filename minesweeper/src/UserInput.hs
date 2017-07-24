module UserInput where
  import Graphics.Gloss.Interface.Pure.Game
  import Board
  import Globals
  import System.Random

  handler :: Event -> Board -> IO (Board)
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b@(Board Win _ _ _) =
    return b
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b@(Board Loss _ _ _) =
    return b
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b = do
    let c = getBoardCoord (x,y) (getWidth) (getHeight) b
    if cellInRange b c then
      return (revealCell b c)
    else
      return b
  handler (EventKey (Char 'r') Up _ _) (Board st sz nm cells) = do
    g <- newStdGen
    return (makeBoard sz nm g)
  handler (EventKey (Char 'e') Up _ _) b = do
    g <- newStdGen
    return (makeBoard easyBoard easyMines g)
  handler (EventKey (Char 'm') Up _ _) b = do
    g <- newStdGen
    return (makeBoard medBoard medMines g)
  handler (EventKey (Char 'h') Up _ _) b = do
    g <- newStdGen
    return (makeBoard hardBoard hardMines g)
  handler _ b = return b

  getBoardCoord :: (Float, Float) -> Float -> Float -> Board -> Coord
  getBoardCoord (x,y) width height (Board st sz nm cells) =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let yOffset = (height/2) in
    let xOffset = (width/2) in
    let adjustedX = x + xOffset in
    let adjustedY = y + yOffset in
    (floor (adjustedX/cellWidth), floor (adjustedY/cellHeight))
