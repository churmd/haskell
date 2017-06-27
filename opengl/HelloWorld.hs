import Graphics.UI.GLUT
import Data.IORef
import Bindings

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _window <- createWindow "Hello World"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less -- the comparison function for depth the buffer
  angle <- newIORef 0
  delta <- newIORef 0.1
  pos <- newIORef (0, 0)
  time <- newIORef 0
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback $= Just (idle angle delta time)
  --addTimerCallback 1000 (idle angle delta time)
  displayCallback $= display angle pos
  --displayCallback $= display time
  mainLoop

{--
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
--}

{--
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
--}
{--
display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive LineLoop $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
--}
{--
display = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0

    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0

    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0
  flush
--}
