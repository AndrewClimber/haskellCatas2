{-
6qyu Have a look at the following numbers.
 n | score
---+-------
 1 |  50
 2 |  150
 3 |  300
 4 |  500
 5 |  750
Can you find a pattern in it? If so, then write a function getScore(n)/get_score(n)/GetScore(n) 
which returns the score for any positive number n:
int getScore(1) = return 50;
int getScore(2) = return 150;
int getScore(3) = return 300;
int getScore(4) = return 500;
int getScore(5) = return 750;
-}

module Codewars.Kata.Sequences where
--import Data.Char(digitToInt)

--getScore :: Integer -> Integer
--getScore n | n > 0 = 50*n + getScore(n-1) | otherwise = n
--getScore n = foldl (\acc x -> acc + 50*x) 0 (tak n)
--tak n = take n $ iterate (+1) 1
--gs 0 = [0]
--gs n  = (50*n):(gs (n-1))


--bu :: Integer -> Integer
--bu n =  truncate $ (fromIntegral (50+50*n)*(fromIntegral n/2))

-- aka
getScore n = (25 + 25 * n) * n

-- codewars
getScore :: Integer -> Integer
getScore = (* 25) . (`div` 4) . (+ (-1)) . (^ 2) . (+ 1) . (* 2)

getScore :: Integer -> Integer
getScore n = 25 * (n * n + n)

import Control.Applicative

getScore :: Integer -> Integer
getScore = (*) <$> (*25) <*> (+1)


getScore :: Integer -> Integer
getScore n = (n * (n + 1) `div` 2) * 50

getScore :: Integer -> Integer
getScore n = n*(50+50*n) `div` 2