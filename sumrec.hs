{-
Write function sumR which returns the sum of values in a given list. Try no to cheat and provide recursive solution.
-}
module SumRecursively where
-- aka
sumR :: [Int] -> Int
sumR [] = 0
sumR [x] = x
sumR (x:xs) = (+) x (sumR xs)

-- codewars . Cool
sumR :: [Int] -> Int
sumR [] = 0
sumR [1] = 1
sumR (x:xs) = x + sumR xs

