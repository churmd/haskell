module Display where
  import Graphics.Gloss
  import qualified Data.Map as Map
  import Board
  import Globals

  render :: Board -> IO (Picture)
  render b@(Board st sz nm cells) =
    let w = getWidth in
    let h = getHeight in
    let top = topText b in
    let bottom = bottomText b in
    let grid = drawGrid b w h in
    return (pictures [top, bottom, grid])

  topText :: Board -> Picture
  topText (Board st sz nm cells) =
    let heightSpace = (getScreenHeight - getHeight)/2 in
    let state = case st of
                  OnGoing -> "      "
                  Win -> "Winner"
                  Loss -> "Loser " in
    let t = translate (-getScreenWidth/4) ((getScreenHeight/2) - heightSpace + 5) $
            scale 0.4 0.4 $
            text state in
    t

  bottomText :: Board -> Picture
  bottomText (Board st sz nm cells) =
    let heightSpace = (getScreenHeight - getHeight)/2 in
    let controls = "Reset - r    Difficultly: Easy - e  Meduim - m  Hard - h" in
    let t = translate (-(getScreenWidth/2)) (-(getScreenHeight/2) + 10) $
            scale 0.11 0.3 $
            text controls in
    t

  cellColor :: Board -> Coord -> Color
  cellColor (Board sz st nm cells) c =
    case Map.lookup c cells of
      Just (Cell False _) -> white
      Just (Cell True (Clear _)) -> greyN 0.5
      Just (Cell True Mine) -> red

  cellText :: Board -> Coord -> String
  cellText (Board sz st nm cells) c =
    case Map.lookup c cells of
      Just (Cell False _) -> ""
      Just (Cell True (Clear n)) -> show n
      Just (Cell True Mine) -> "X"

-- uses bottom left of screen as (0,0) on board
  drawGrid :: Board -> Float -> Float -> Picture
  drawGrid b@(Board st sz nm cells) width height =
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
