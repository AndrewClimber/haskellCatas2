{-
6 kyu Alphabetized
The alphabetized kata
Re-order the characters of a string, so that they are concatenated into a new string in 
    "case-insensitively-alphabetical-order-of-appearance" order. 
    Whitespace and punctuation shall simply be removed!

The input is restricted to contain no numerals and only words 
containing the english alphabet letters.

Example:

alphabetized "The Holy Bible" -- "BbeehHilloTy"
-}
module Alphabetized.Kata (alphabetized) where
import Data.List
import Data.Char

myMax :: Ord a => a -> a -> a
myMax x y | x > y     = x
          | otherwise = y

alphabetized :: String -> String
alphabetized [] = []
alphabetized (x:xs) = (toUpper x):alphabetized xs
--alphabetized s = take length(s) 

qsort [] = []
qsort (x:xs) = (qsort [y | y <- xs, y<=x]) ++ [x] ++ (qsort [y | y <- xs, y>x])

d = [(1,'A'),(2,'a'),(3,'B'),(4,'b'),(5,'C'),(6,'c')]
x = [(3,'B'),(5,'C'),(4,'b')]


--s = zip [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52] "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" 
--s = zip [1..52] "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" 
bigLetter = zip [1,3..52] ['A'..'Z']
smallLetter = zip [2,4..52] ['a'..'z']



