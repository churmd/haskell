module Board where

type Tile = (Int, Int)

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

movePiece :: [Tile] -> Tile -> Tile -> [Tile]
movePiece ts old new = new : (filter (/= old) ts)

moveWhitePiece :: Board -> Tile -> Tile -> Board
moveWhitePiece b old new = b {whitePieces = updatedWP}
  where
    updatedWP = movePiece (whitePieces b) old new

moveBlackPiece :: Board -> Tile -> Tile -> Board
moveBlackPiece b old new = b {blackPieces = updatedBP}
  where
    updatedBP = movePiece (blackPieces b) old new

addFire :: Board -> Tile -> Board
addFire b t = b {fires = updatedFires}
  where
    updatedFires = t:(fires b)
