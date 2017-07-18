module Display where
  import Graphics.Gloss
  import Board

  width :: Float
  width = 800

  height :: Float
  height = 800

  render :: Board -> IO (Picture)
  render b@(Board st sz cells) =
    return (drawGrid b width height)


  drawGrid :: Board -> Float -> Float -> Picture
  drawGrid (Board st sz cells) width height =
    let cellWidth = width / (fromIntegral sz) in
    let cellHeight = height / (fromIntegral sz) in
    let hLines = [color black (line [(-(height/2),yVal), (height/2, yVal)]) | y <- [1..(fromIntegral sz :: Float) -1], let yVal = (y*cellHeight) - (height/2)]
    in
    undefined
