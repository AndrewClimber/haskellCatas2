{-
Have a look at the following numbers.

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


getScore :: Integer -> Integer
--getScore n | n > 0 = 50*n + getScore(n-1) | otherwise = n
--getScore n = foldl (\acc x -> acc + 50*x) 0 (tak n)
getScore n = (foldl (\acc x -> acc + 50*x) 0 [n])
gs n = foldl (\acc x -> acc + 50*x) 50 [n]
--tak n = take n $ iterate (+1) 1
tak n = take n $ iterate (+1) 1

--getS n =  foldl (\x y-> 50*x + getS (y-1)) 1 (ta n)

getS n =  foldl (\acc x -> acc + 50*x) 0 (ta n)
ta n = take n $ iterate (+1) 1