{-
Complete the solution so that it returns a formatted string. The return value should equal "Value is VALUE" 
where value is a 5 digit padded number.

Example:

solution 5  -- should return "Value is 00005"
-}
module Codewars.Kata.PaddedNumbers where

-- aka    
solution :: Int -> String
solution n = "Value is " ++ dupc '0' (5-(length $ show n)) ++ show n where
    dupc _ 0 = []
    dupc c n = c:dupc c (n-1) 

-- codewars
import Text.Printf

solution' :: Int -> String
solution' = printf "Value is %05d"     

----
solution :: Int -> String
solution n = let s = show n in "Value is " ++ (replicate (5 - length s) '0') ++ s  

----------
solution :: Int -> String
solution n = let s = show n in "Value is " ++ drop (length s) ("00000" ++ s)