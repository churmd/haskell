module MultPersist where

{--
Repeatedly multiply the digits of a number together until the number is a
single digit. Count how many multiplications that took.
--}

-- | Calculate the multiplicative persistance of a number >= 0
multPer :: Integer -> Integer
multPer x | length(chars) == 1 = 0
          | otherwise = 1 + multPer(multDigits(chars))
          where
            chars = show(x)

multDigits :: String -> Integer
multDigits s =  foldl (*) 1 $ map charToInteger s

charToInteger :: Char -> Integer
charToInteger '0' = 0
charToInteger '1' = 1
charToInteger '2' = 2
charToInteger '3' = 3
charToInteger '4' = 4
charToInteger '5' = 5
charToInteger '6' = 6
charToInteger '7' = 7
charToInteger '8' = 8
charToInteger '9' = 9
charToInteger _ = error "Not a number character"

-- | Calculates the multiplicative persistance of every number from [1..]
multPersistList :: [Integer]
multPersistList = [multPer x | x <- [1..] ]

-- | A list of every number from 1 to inifity in a tuple with its
-- | multiplicative persistance
multPersistTuple :: [(Integer, Integer)]
multPersistTuple = zip [1..] multPersistList

-- | Print a table of a number (1..) with its  multiplicative persistance
prettyPrintMultPer :: IO ()
prettyPrintMultPer = do
  putStrLn title
  (printNext multPersistTuple)

printNext :: [(Integer, Integer)] -> IO ()
printNext [] = do
  putStr ""
printNext (x : xs) = do
  putStrLn (printTuple x)
  (printNext xs)

ppN :: String
ppN = "Number"

ppMP :: String
ppMP = "Multiplicative Persistance"

ppSplit :: String
ppSplit = " | "

title :: String
title = ppN ++ ppSplit ++ ppMP

printTuple :: (Integer, Integer) -> String
printTuple (x, y) = l ++ ppSplit ++ r
  where
    l = pad (show(x)) (length(ppN))
    r = pad (show(y)) (length(ppMP))

pad :: String -> Int -> String
pad s n | sLen > n = s
        | otherwise = s ++ (foldl (++) "" $ take (n - sLen) $ repeat " ")
        where
          sLen = length(s)
