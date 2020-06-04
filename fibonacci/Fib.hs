module Fib where
import Data.List

{--
A series of fibonacci functions improving performance.

The lazy list comprehension appears to be the most performant, which surprises
me as I assumed the list would take up more memory than the tail recurion
version.

To test run in GHCI and run the command :set +s
Note: results will be cached so running the same function or function that
builds upon a previous result may have unintended speed ups.
--}

fibRecursion :: Integer -> Integer
fibRecursion 0 = 0
fibRecursion 1 = 1
fibRecursion n = fibRecursion(n-2) + fibRecursion(n-1)

fibTailRecursion :: Integer -> Integer
fibTailRecursion n = fibTailRecursionHelper n (0,1)

fibTailRecursionHelper :: Integer -> (Integer, Integer) -> Integer
fibTailRecursionHelper 0 (a, b) = a
fibTailRecursionHelper 1 (a, b) = b
fibTailRecursionHelper n (a, b) = fibTailRecursionHelper (n-1) (b, a + b)

fibLazyListComprehension :: [Integer]
fibLazyListComprehension =
  0:1:[ a + b |
    (a, b) <- zip fibLazyListComprehension (tail fibLazyListComprehension)]

fibLazyListComprehensionElemet :: Integer -> Integer
fibLazyListComprehensionElemet n = genericIndex fibLazyListComprehension n
