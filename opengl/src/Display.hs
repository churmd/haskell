module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Cube
import Points

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    --rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 1 0 -- changed y-component a bit to show off cube corners
    scale 0.7 0.7 (0.7::GLfloat)
    forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      cube 0.1
      color $ Color3 (0::GLfloat) 0 0 -- set outline color to black
      cubeFrame 0.1 -- draw the outline
  swapBuffers


{--
display:: IORef Int -> DisplayCallback
display time = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  t <- readIORef time
  preservingMatrix $ do
    color $ Color3 1 0 (0::GLfloat)
    translate $ Vector3 (path!!t) 0 0
    cube 0.1

  swapBuffers
--}

idle :: IORef GLfloat -> IORef GLfloat -> IORef Int -> IdleCallback
idle angle delta time = do
  d <- get delta
  angle $~! (+ d)
  t <- get time
  if t == 8 then
    time $~! \x -> 1
  else
     time $~! (+ 1)
  postRedisplay Nothing
