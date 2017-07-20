module UserInput where
  import Graphics.Gloss.Interface.Pure.Game
  import Board
  import Display

  handler :: Event -> Board -> IO (Board)
  handler (EventKey (MouseButton LeftButton) Up _ (x, y)) b = do
    let c = getBoardCoord (x,y) (getWidth) (getHeight) b
    putStrLn $ show (x,y)
    putStrLn $ show c
    return (revealCell b c)
  handler _ b = return b

  getBoardCoord :: (Float, Float) -> Float -> Float -> Board -> Coord
  getBoardCoord (x,y) width height (Board st sz cells) =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let yOffset = (height/2) in
    let xOffset = (width/2) in
    let adjustedX = x + xOffset in
    let adjustedY = y + yOffset in
    (floor (adjustedX/cellWidth), floor (adjustedY/cellHeight))
