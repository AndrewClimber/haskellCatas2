{-
Create a function with two arguments that will return a list of length (n) with multiples of (x).

Assume both the given number and the number of times to count will be positive numbers greater than 0.

Return the results as an array (or list in Python, Haskell or Elixir).

Examples:

countBy 1 10 `shouldBe` [1,2,3,4,5,6,7,8,9,10]
countBy 2  5 `shouldBe` [2,4,6,8,10]
-}
module Codewars.Kata.Count where

-- мой
countBy :: Integer -> Integer -> [Integer]
countBy x n = [x,(x+x)..n*x]

-- codewars
countBy :: Integer -> Int -> [Integer]
countBy x n = take n [x, x + x..]

countBy :: Integer -> Int -> [Integer]
countBy x n = take n [x,2 * x..]

countBy :: Integer -> Integer -> [Integer]
countBy x n = map (*x) [1..n]
