module Display where
  import Graphics.Gloss
  import Board

  getWidth :: Float
  getWidth = 300

  getHeight :: Float
  getHeight = 600

  render :: Board -> IO (Picture)
  render b@(Board st sz cells) =
    return (drawGrid b getWidth getHeight)


  drawGrid :: Board -> Float -> Float -> Picture
  drawGrid (Board st sz cells) width height =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let hLines = [color black (line [(-(height/2),yVal), (height/2, yVal)]) | y <- [1..(fromIntegral sz :: Float) -1], let yVal = (y*cellHeight) - (height/2)]
    in
    let vLines = [color black (line [(xVal,-(width/2)), (xVal,(width/2))]) | x <- [1..(fromIntegral sz :: Float) -1], let xVal = (x*cellWidth) - (width/2)]
    in
    pictures (hLines ++ vLines)
