module Tribonacci where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n =  a:b:c:zipWith (+) tribonacci (tail tribonacci(a, b, c))