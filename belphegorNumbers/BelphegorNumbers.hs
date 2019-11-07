module BelphegorNumbers where

{-
A Belphegor number (also known as a Beelphegor number or a beastly palindromic
prime) is a palindromic number of the form 1(0...)666(0...)1. Numbers of this
form are named after the demon Belphegor (or Beelphegor), one of the seven
princes of Hell and the demon of inventiveness.

Belphegor prime has a number of numerological properties, including a central
beast number 666 which is surrounded on each side by 13 (a number traditionally
associated with bad luck) zeros and an overall decimal number length of 31 -
which is 13 backwards.
-}

-- Appears faster
calculationA :: Integer -> Integer
calculationA n = (10 ^ ((2 * n) + 4)) + (666 * (10 ^ (n + 1))) + 1

calculationB :: Integer -> Integer
calculationB n = ((10 ^ (n + 3)) + 666) * (10 ^ (n + 1)) + 1

-- Appears faster
belphegor :: Integer -> Integer
belphegor n |n < 0 = error "Cannot get a negative index of a sequence"
            | otherwise = calculationA n

belphegorSequence = [belphegor x | x <- [0..]]

belphegorPrime = belphegor 13
