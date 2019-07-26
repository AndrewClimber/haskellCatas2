{-
Create a function taking a positive integer as its parameter and returning a string containing the Roman Numeral 
representation of that integer.
Modern Roman numerals are written by expressing each digit separately starting with the left most digit and 
skipping any digit with a value of zero. In Roman numerals 1990 is rendered: 
1000=M, 900=CM, 90=XC; resulting in MCMXC. 2008 is written as 2000=MM, 8=VIII; or MMVIII. 
1666 uses each Roman symbol in descending order: MDCLXVI.

Example:

solution 1000 -- should return "M"
Help:

Symbol    Value
I          1
V          5
X          10
L          50
C          100
D          500
M          1,000
-}
module RomanNumerals where
--import Data.List

convMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
              (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
              (4,"IV"), (1,"I")]

numToSubtract :: Integer -> (Integer, String)
numToSubtract x = head (filter (lessThan x) convMap)

solution :: Integer -> String
solution x 
  | x == 0 = "N"
  | otherwise = (snd . numToSubtract $ x) ++ toRoman'(nextNum)
      where nextNum = x - (fst. numToSubtract $ x)   

toRoman' :: Integer -> String
toRoman' x 
  | x == 0 = ""
  | x > 0 = (snd . numToSubtract $ x) ++ toRoman'(nextNum)
      where nextNum = x - (fst. numToSubtract $ x)

lessThan :: Integer -> (Integer, String) -> Bool
lessThan n (a, b)
  | a <= n = True
  | otherwise = False      

--------------- codewars
-- SUper !
numerals :: [(String, Integer)]
numerals = zip ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" ]
               [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

solution :: Integer -> String
solution 0 = ""
solution n = k ++ solution (n - v)
             where (k, v) = head $ filter ((<=n).snd) numerals
