module Points where

import Graphics.Rendering.OpenGL

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n

path :: [GLfloat]
path = [1,2,3,4,5,4,3,2,1]
