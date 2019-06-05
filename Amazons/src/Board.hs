module Board where

type Tile = (Int, Int)

data Board = Board {
  width :: Int,
  height :: Int,
  blackPieces :: [Tile],
  whitePieces :: [Tile],
  fires :: [Tile]
}

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

instance Show Board where
  show b@(Board w h bp wp f) =
    board ++ "\n" ++ bottomNums
      where
        lineNums = reverse [0..(h-1)]
        lineList = map (\l -> showLine b l) lineNums
        board = joinStrings lineList "\n"
        bottomNums = bottomLabels w

bottomLabels :: Int -> String
bottomLabels width = " " ++ (joinStrings labels " ")
  where
    labels = [" " ++ (show x) ++ " " | x <- [0..(width-1)]]

showLine :: Board -> Int  -> String
showLine b@(Board w h bp wp f) l =
  (show l) ++ line
    where
      coords = [(x,l) | x <- [0..(w-1)]]
      tiles = map (\coord -> showCoord b coord) coords
      line = joinStrings tiles " "


showCoord :: Board -> Tile -> String
showCoord (Board w h bp wp f) t
  | isFire = "[F]"
  | isWhite = "[W]"
  | isBlack = "[B]"
  | otherwise = "[ ]"
    where
      isFire = elem t f
      isWhite = elem t wp
      isBlack = elem t bp

joinStrings :: [String] -> String -> String
joinStrings [] _ = ""
joinStrings [x] _ = x
joinStrings (x:y:zs) sep = x ++ sep ++ (joinStrings (y:zs) sep)

testBoard :: Board
testBoard = Board 4 4 [(0,0), (3,1)] [(1,3), (3,3)] [(0,3),(2,3),(0,2),(2,2),(3,2),(0,1),(1,1),(2,1),(1,0),(2,0),(3,0)]
