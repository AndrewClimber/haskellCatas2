{-8 kyu
Square(n) Sum
Complete the square sum function so that it squares each number passed into it and then sums the results together.

For example, for [1, 2, 2] it should return 9 because 1^2 + 2^2 + 2^2 = 9.

-}

module SquareSum where

-- aka    
squareSum :: [Integer] -> Integer
squareSum s = sum $ map (^2) s

-- codewars
squareSum :: [Integer] -> Integer
squareSum = sum . map (^2)


squareSum :: [Integer] -> Integer
squareSum = foldr ((+) . (^2)) 0

squareSum :: [Integer] -> Integer
squareSum x = sum [i^2 | i<-x]
