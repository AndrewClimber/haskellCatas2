{-
6 kyu Alphabetized
The alphabetized kata
Re-order the characters of a string, so that they are concatenated into a new string in 
    "case-insensitively-alphabetical-order-of-appearance" order. 
    Whitespace and punctuation shall simply be removed!

The input is restricted to contain no numerals and only words 
containing the english alphabet letters.

Example:

alphabetized "The Holy Bible" -- 
"BbeehHilloTy"
"BbeeHhilloTy"
-}
module Alphabetized.Kata (alphabetized) where

import Data.List
import Data.Maybe
import Data.Char


-- aka
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare (toUpper b1) (toUpper b2)

alphabetized :: String -> String
alphabetized s =  snd $ unzip $ sortBy sortGT $ zip (map myOrd (f s)) (f s)   where
    f = filter (`elem` ['A'..'Z']++['a'..'z'])
    myOrd c =  fromJust $ elemIndex (toUpper c) ['A'..'Z']

    

---- codewars
import Data.Char (isAlpha, toLower)
import Data.List (sortOn)

alphabetized' :: String -> String
alphabetized' = sortOn toLower . filter isAlpha    

----------------------------------------------
import Data.Char (isLetter, toLower)
import Data.Function (on)
import Data.List (sortBy)

alphabetized :: String -> String
alphabetized = sortBy (compare `on` toLower) . filter isLetter

-----------------------------------------
import Data.List (sortBy)
import Data.Char (isAlpha, toLower)

alphabetized :: String -> String
alphabetized = dropWhile (not . isAlpha) . sortBy (\x y -> toLower x `compare` toLower y)

-----------------------------------------
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (toLower, isLetter)

alphabetized :: String -> String
alphabetized = sortBy (comparing toLower) . filter isLetter

---------------------------
import Data.Char
import Data.List

alphabetized :: String -> String
alphabetized = sortOn order . filter isAlpha

order::Char->Int
order c
  | ord c >= 97 = ord c - 97
  | otherwise = ord c - 65
--------------------------------------
import Data.Char
import Data.List
alphabetized :: String -> String
alphabetized = sortBy go . filter isLetter
  where
    go a b = compare (toLower a) (toLower b)
---------------------------------------
    