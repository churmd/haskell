module Pathing where
import Board

type PathFunction = Tile -> Int -> Int -> [Tile]

allPathFunctions :: [PathFunction]
allPathFunctions =
  [upPath, downPath, leftPath, rightPath, rightUpPath,
  rightDownPath, leftUpPath, leftDownPath]

upPath :: PathFunction
upPath t w h = path t (\(x,y) -> (x,y+1)) (\(x,y) -> y==h)

downPath :: PathFunction
downPath t w h = path t (\(x,y) -> (x,y-1)) (\(x,y) -> y<0)

rightPath :: PathFunction
rightPath t w h = path t (\(x,y) -> (x+1,y)) (\(x,y) -> x==w)

leftPath :: PathFunction
leftPath t w h = path t (\(x,y) -> (x-1,y)) (\(x,y) -> x<0)

rightUpPath :: PathFunction
rightUpPath t w h = path t (\(x,y) -> (x+1,y+1)) (\(x,y) -> x==w || y==h)

rightDownPath :: PathFunction
rightDownPath t w h = path t (\(x,y) -> (x+1,y-1)) (\(x,y) -> x==w || y<0)

leftDownPath :: PathFunction
leftDownPath t w h = path t (\(x,y) -> (x-1,y-1)) (\(x,y) -> x<0 || y<0)

leftUpPath :: PathFunction
leftUpPath t w h = path t (\(x,y) -> (x-1,y+1)) (\(x,y) -> x<0 || y==h)

path :: Tile -> (Tile -> Tile) -> (Tile -> Bool) -> [Tile]
path t inc stop | not $ stop nextTile = nextTile:(path nextTile inc stop)
                | otherwise = []
                  where
                    nextTile = inc t
