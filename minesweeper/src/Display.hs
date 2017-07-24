module Display where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import Board
  import Globals

  render :: Board -> IO (Picture)
  render b@(Board st sz cells) =
    let w = getWidth in
    let h = getHeight in
    let overlay = gameOver w h b in
    let grid = drawGrid b w h in
    return (pictures [grid, overlay])

  gameOver :: Float -> Float -> Board -> Picture
  gameOver width height (Board OnGoing sz cells) = Blank
  gameOver width height (Board Win sz cells) =
    let box = color (withAlpha 0.5 green) $
              rectangleSolid width (height/4) in
    pictures [box, (text "Winner")]
  gameOver width height (Board Loss sz cells) =
    let box = color (withAlpha 0.5 red) $
              rectangleSolid width (height/4) in
    pictures [box, (text "Loser")]

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
                    [ -- grid square
                      translate (sqX + xOffset) (sqY + yOffset) $
                      color (cellColor b (truncate x, truncate y)) $
                      rectangleSolid cellWidth cellHeight
                      , -- square outline
                      translate (sqX + xOffset) (sqY + yOffset) $
                      color black $
                      rectangleWire cellWidth cellHeight
                      , -- text in square, first line centers the text
                      translate(-cellWidth/8) (-cellHeight/8) $
                      translate (sqX + xOffset) (sqY + yOffset) $
                      scale (2*(cellWidth/width)) (2*(cellHeight/height)) $
                      color black $
                      text (cellText b (truncate x, truncate y))
                    ]
                  | x <- [0..(fromIntegral sz :: Float)-1],
                  y <- [0..(fromIntegral sz :: Float)-1],
                  let sqX = x*cellWidth,
                  let sqY = y*cellHeight
                  ] in
    pictures squares
