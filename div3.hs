{-
A trick I learned in elementary school to determine whether or not a number was divisible by three is to add 
all of the integers in the number together and to divide the resulting sum by three. If there is no remainder from dividing the sum by three, 
then the original number is divisible by three as well.

Given a series of numbers as a string, determine if the number represented by the string is divisible by three.

You can expect all test case arguments to be strings representing values greater than 0.

Example:

divisibleByThree('123') // true
divisibleByThree('8409') // true
divisibleByThree('100853') // false
divisibleByThree('33333333') // true
divisibleByThree('7') // false
-}
-- моё
divisibleByThree :: String -> Bool
divisibleByThree  = strToArr  where
    strToArr (s:sx) = sum(map read ([[x] | x <- s:sx]) :: [Int]) `mod` 3 == 0

-- codewars
divisibleByThree str = n `mod` 3 == 0
  where n = read str :: Integer


import Data.Char (digitToInt)

divisibleByThree :: String -> Bool
divisibleByThree = (== 0) . (`mod` 3) . sum . map digitToInt  


divisibleByThree [n] = elem n "369"
divisibleByThree chars = divisibleByThree (show $ sum $ map digitToInt chars)

divisibleByThree = (== 0) . (`mod` 3) . read