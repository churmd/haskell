module Fib where

fib :: [Integer]
fib = 1:1:[ a + b | (a, b) <- zip fib (tail fib)]
