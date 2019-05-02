module Board where

type Tile = (Int, Int)
data Player = Black | White deriving (Show, Eq)
data Step = Move | Fire Tile deriving (Show, Eq)

data Board = Board {
  width :: Int,
  height :: Int,
  blackPieces :: [Tile],
  whitePieces :: [Tile],
  fires :: [Tile]
} deriving (Show)

emptySpace :: Board -> Tile -> Bool
emptySpace b@(Board _ _ bp wp f) c = notInBP && notInWP && notInF
  where
    notInBP = notElem c bp
    notInWP = notElem c wp
    notInF = notElem c f

inBounds :: Board -> Tile -> Bool
inBounds (Board w h _ _ _) (x, y) = xGreater && yGreater && xLess && yLess
  where
    xGreater = x >= 0
    yGreater = y >= 0
    xLess = x < w
    yLess = y < h
