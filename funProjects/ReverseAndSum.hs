module ReverseAndSum where

import Data.List
import Data.Maybe

isPalindrome :: Integer -> Bool
isPalindrome n = forward == backwards
  where
    forward = show n
    backwards = reverse forward

step :: Integer -> Integer
step n = n + nReverse
  where
    nString = show n
    nStringReverse = reverse nString
    nReverse = read nStringReverse :: Integer

{-
Game :
  - Take a number = n
  - Reverse n = rn
  - Add these two values n + rn = x
  - If x is a palindrome return x else play the game again with x
-}

reverseAndSum :: Integer -> Integer
reverseAndSum n | n < 0 = error "Positve numbers only"
                | isPalindrome n = n
                | otherwise = reverseAndSum $ step n

reverseAndSumLimited :: Integer -> Integer -> Maybe Integer
reverseAndSumLimited n l | n < 0 = error "Positve numbers only"
                         | l < 0 = error "Limit must be positive"
                         | l == 0 = Nothing
                         | isPalindrome n = Just n
                         | otherwise = reverseAndSumLimited (step n) (l-1)

{-
A number that does not eventually become a palindrome when repeatedly reversed
and summed is a lychrel number. The problem of is a number a lychrel number
has apparently been solved in lower bases but base 10 is the first where it has
not.
 -}

{-
Finding "lychrel" numbers by limiting the number of reverse and sum cycles.
-}
lychrelLimited :: Integer -> [Integer]
lychrelLimited l = filter (\x -> isJust $ lookup x notLychrel) [0..]
  where
   notLychrel = [(x, isJust result) | x <- [0..], let result = reverseAndSumLimited x l]

lychrelLimited100 :: [Integer]
lychrelLimited100 = lychrelLimited 100
