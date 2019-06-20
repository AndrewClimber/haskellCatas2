{-
In this kata you should simply determine, whether a given year is a leap year or not. In case you don't know the rules, here they are:

years divisible by 4 are leap years
but years divisible by 100 are not leap years
but years divisible by 400 are leap years
Additional Notes:

Only valid years (positive integers) will be tested, so you don't have to validate them
Examples can be found in the test fixture.

-}

module Codewars.Kata.Leap where

-- мой
isLeapYear x = (sum $ [fromEnum $ (x `mod` 400 == 0), fromEnum $ (x `mod` 4 == 0), fromEnum $ (x `mod` 100 /= 0)]) >= 2
 
-- codewars
isLeapYear' x = mod x 4==0 && mod x 100/=0 || mod x 400==0

isLeapYear'' x = foldl1 (/=) $ map ((== 0) . mod x) [4, 100, 400]
