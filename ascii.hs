{-
Write a function which takes a number and returns the corresponding ASCII char for that value.

Example:

get_char(65) # => 'A'
For ASCII table, you can refer to http://www.asciitable.com/

-}

module Ascii where

-- моё    
import Data.Char
getChar' :: Int -> Char
getChar' = chr

-- с codewars
getChar'' :: Int -> Char
getChar'' = toEnum