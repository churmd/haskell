module Display where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import Board

  getWidth :: Float
  getWidth = 600

  getHeight :: Float
  getHeight = 600

  render :: Board -> IO (Picture)
  render b@(Board st sz cells) =
    return (drawGrid b (getWidth-4) (getHeight-4))

  cellColor :: Board -> Coord -> Color
  cellColor (Board sz st cells) c =
    case Map.lookup c cells of
      Just (Cell False _) -> white
      Just (Cell True (Clear _)) -> greyN 0.5
      Just (Cell True Mine) -> red

  cellText :: Board -> Coord -> String
  cellText (Board sz st cells) c =
    case Map.lookup c cells of
      Just (Cell False _) -> ""
      Just (Cell True (Clear n)) -> show n
      Just (Cell True Mine) -> "X"

-- uses bottom left of screen as (0,0) on board
  drawGrid :: Board -> Float -> Float -> Picture
  drawGrid b@(Board st sz cells) width height =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let yOffset = (-height/2) + (cellHeight/2) in
    let xOffset = (-width/2) + (cellWidth/2) in
    let squares = [ pictures
                    [
                      translate (sqX + xOffset) (sqY + yOffset) $
                      color (cellColor b (truncate x, truncate y)) $
                      rectangleSolid cellWidth cellHeight
                      , translate (sqX + xOffset) (sqY + yOffset) $
                      color black $
                      rectangleWire cellWidth cellHeight
                      , translate(-cellWidth/8) (-cellHeight/8) $ --center text
                      translate (sqX + xOffset) (sqY + yOffset) $
                      scale 0.5 0.5 $
                      color black $
                      text (cellText b (truncate x, truncate y))
                    ]
                  | x <- [0..(fromIntegral sz :: Float)-1],
                  y <- [0..(fromIntegral sz :: Float)-1],
                  let sqX = x*cellWidth,
                  let sqY = y*cellHeight
                  ] in
    pictures squares
