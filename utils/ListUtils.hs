module ListUtils (
  append,
  prepend,
  len,
  rev,
  equal,
  isPrefix,
  isSuffix,
  indexOfSubList,
  contains,
  replaceFirst,
  replaceAll
) where

{-|
  Add an element to the beginning of a list.
-}
prepend :: a -> [a] -> [a]
prepend x ys = x : ys

{-|
  Add an element to the end of a list.
-}
append :: [a] -> a -> [a]
append xs y = xs ++ [y]

{-|
  Returns the length of the list.
-}
len :: [a] -> Integer
len [] = 0
len (x : xs) = 1 + (len xs)

{-|
  Reverses the list.
-}
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = append (rev xs) x

{-|
  Returns True if the lists are equal, otherwise False.
-}
equal :: (Eq a) => [a] -> [a] -> Bool
equal [] [] = True
equal [] _ = False
equal _ [] = False
equal (x : xs) (y : ys) =
  if x == y
    then equal xs ys
    else False

{-|
  Returns True if the second list is the beginning of the first,
  otherwise False.
-}
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix _ [] = True
isPrefix [] (y:ys) = False
isPrefix (x:xs) (y:ys) =
  if x == y
    then isPrefix xs ys
    else False

{-|
  Returns True if the second list is the end of the first,
  otherwise False.
-}
isSuffix :: (Eq a) => [a] -> [a] -> Bool
isSuffix _ [] = True
isSuffix [] _ = False
isSuffix l1 l2
  | lenL1 < lenL2 = False
  | lenL1 == lenL2 = equal l1 l2
  | otherwise = isSuffix (tail l1) l2
    where
      lenL1 = len l1
      lenL2 = len l2

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

{-|
  Returns True if the first list contains the second list within it,
  otherwise false.
-}
contains :: (Eq a) => [a] -> [a] -> Bool
contains mainList subList = indexOfSubList mainList subList /= -1

{-|
  Replaces the first occurence of an element with another element.
  If no matching element is found, the original list is returned.
  e.g. replaceFirst [1,1,2,3] 1 5 = [5,1,2,3]
-}
replaceFirst :: (Eq a) => [a] -> a -> a -> [a]
replaceFirst [] _ _ = []
replaceFirst (x : xs) toReplace replacement =
  if x == toReplace
    then replacement : xs
    else x : (replaceFirst xs toReplace replacement)

{-|
  Replaces the all occurences of an element with another element.
  e.g. replaceFirst [1,1,2,3] 1 5 = [5,5,2,3]
-}
replaceAll :: (Eq a) => [a] -> a -> a -> [a]
replaceAll [] _ _ = []
replaceAll (x : xs) toReplace replacement =
  if x == toReplace
    then replacement : (replaceAll xs toReplace replacement)
    else x : (replaceAll xs toReplace replacement)
