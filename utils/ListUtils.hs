module ListUtils (
  isPrefix,
  indexOfSubList,
) where

{-|
  Returns True if the second list is the beginning of the first,
  otherwise False.
-}
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix _ [] = False
isPrefix [] (y:ys) = False
isPrefix (x:xs) (y:ys) =
  if x == y
    then isPrefix xs ys
    else False

{-|
  Searches the main list for the first occurence of the sub list.
  Returns the index of where the sublist begins in the main list.
  Returns -1 if the sublist is not in the main list.
-}
indexOfSubList :: (Eq a) => [a] -> [a] -> Int
indexOfSubList mainList subList =
  indexOfSubListHelper mainList subList 0

indexOfSubListHelper :: (Eq a) => [a] -> [a] -> Int -> Int
indexOfSubListHelper [] (y:ys) _ = -1
indexOfSubListHelper mainList@(x:xs) subList acc =
  if isPrefix mainList subList
    then acc
    else indexOfSubListHelper xs subList (acc + 1)
