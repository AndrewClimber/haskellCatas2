{-
6 kyu. Persistent Bugger
Write a function, persistence, that takes in a positive parameter num and returns its multiplicative persistence, 
which is the number of times you must multiply the digits in num until you reach a single digit.

For example:

 persistence 39 -- returns 3, because 3*9=27, 2*7=14, 1*4=4
                -- and 4 has only one digit

 persistence 999 -- returns 4, because 9*9*9=729, 7*2*9=126,
                 -- 1*2*6=12, and finally 1*2=2

 persistence 4 -- returns 0, because 4 is already a one-digit number
-}

module Codewars.G.Persistence where

-- aka    
persistence :: Int -> Int
persistence n = multiply n 0 where
    multiply n k =  if  mul > 0 then multiply mul (k+1) else k where
        mul = (mul' $ show n) where
            mul' str  | length str == 1 = 0
                      | length str > 1 && '0' `elem` str = 1
                      | otherwise = product (map read ([ [x] | x <- str]) :: [Int])
                  

-- codewars
import Data.Char (digitToInt)

persistence :: Int -> Int
persistence n = if n < 10 then 0 else 1 + persistence (product $ map digitToInt $ show n)

---
persistence :: Int -> Int
persistence n = 
  let digits = map (\x -> read $ [x]) $ show n in
      if length digits == 1
         then 0
         else 1 + persistence (product digits)

--- хорошое решение без конвертации числа в строку и обратно
persistence :: Int -> Int
persistence n
       | n < 10    = 0
       | otherwise = 1 + persistence (digitProduct n)
         
digitProduct :: Int -> Int
digitProduct n
    | n < 10    = n
    | otherwise = r * digitProduct q
    where
        (q, r) = n `divMod` 10         