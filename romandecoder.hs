{-
Create a function that takes a Roman numeral as its argument and returns its value as a numeric decimal integer. You don't need to validate the form of the Roman numeral.

Modern Roman numerals are written by expressing each decimal digit of the number to be encoded separately, starting with the leftmost digit and skipping any 0s. So 1990 is rendered "MCMXC" (1000 = M, 900 = CM, 90 = XC) and 2008 is rendered "MMVIII" (2000 = MM, 8 = VIII). The Roman numeral for 1666, "MDCLXVI", uses each letter in descending order.

Example:

solution "XXI" -- should return 21
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

module Roman where

solution :: String -> Int
-- aka
solution  = ( foldl (\x acc -> if x >= acc then acc + x else acc - x) 0 ) . (map romanDecoder)
romanDecoder romNum = 
    case romNum of
        'I' -> 1
        'V' -> 5
        'X' -> 10
        'L' -> 50
        'C' -> 100
        'D' -> 500
        'M' -> 1000

-- codewars
solution :: String -> Int
solution = sum . map re . groupBy (<) . map romanToInt
  where re [x] = x
        re [x,y] = y - x

romanToInt :: Char -> Int
romanToInt c =
    case c of
      'I' -> 1
      'V' -> 5
      'X' -> 10
      'L' -> 50
      'C' -> 100
      'D' -> 500
      'M' -> 1000
      _ -> 0


--- понравилось сравнение текущего элемента с предыдущим : head xs > x      
solution :: String -> Int
solution "" = 0
solution (x:xs) = if length (xs) > 0 && numToInt (head xs) > numToInt x then (numToInt (head xs) - (numToInt x)) + (solution (tail xs)) else numToInt x + (solution (xs))
    where numToInt x = fromJust (lookup x [('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)])      