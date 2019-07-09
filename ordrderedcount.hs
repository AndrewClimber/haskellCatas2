{-
7 kyu. Ordered Count of Characters
Count the number of occurrences of each character and return it as a list of tuples in order of appearance.

Example:
orderedCount "abracadabra" == [('a', 5), ('b', 2), ('r', 2), ('c', 1), ('d', 1)]
-}
module Kata where
import Data.List

-- aka
orderedCount :: String -> [(Char, Int)]
orderedCount s = zip (nub s) (countAll s) where
  countAll [] = []                
  countAll (x:xs) =  (count (x:xs) (head $ nub (x:xs)) 0):countAll (filter (/=x) xs)
  count [] _ n = n
  count (x:xs) c n | c == x = count xs c (n+1) 
                   | otherwise =  count xs c n

-- codewars

orderedCount :: String -> [(Char, Int)]
orderedCount s = [(c, length $ filter (==c) s) | c <- nub s]

--------------------
orderedCount :: String -> [(Char, Int)]
orderedCount s = [(x, length(elemIndices x s)) | x <- nub s]

-------------
orderedCount :: String -> [(Char, Int)]
orderedCount s = map (\c -> (c, length (filter ((==) c) s))) (nub s)