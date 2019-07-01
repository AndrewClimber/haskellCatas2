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

persistence :: Int -> Int
persistence n = multiply n 1 where
    multiply n k =  if  mul > 0 then multiply mul (k+1) else k where
        mul = (mul' $ show n) where
            mul' str  | length str == 1 = 0
                      | otherwise = product (map read ([[x] | x <- str]) :: [Int])
                  

multiply n k =  if  mul > 0 then multiply mul (k+1) else k where
    mul = (n `div` 10) * (n `mod` 10)

--strToArr (s:sx) =  (map read ([[x] | x <- s:sx]) :: [Int])

strToArr (s:sx) =  ([if [x] == "0" then "10" else [x] | x <- s:sx])


mul str  | length str == 1 = 0 
         | otherwise = product (map read ([ [x] | x <- str]) :: [Int])

