{-
Return the number (count) of vowels in the given string.

We will consider a, e, i, o, and u as vowels for this Kata.

The input string will only consist of lower case letters and/or spaces.
-}
module Codewars.Kata.Vowel where

-- мой
getCount :: String -> Int
getCount str = length [c | c <- str, c `elem` "aeiouAEIOU"]

-- codewars
getCount :: String -> Int
getCount = length . filter (`elem` "aeiouAEIOU")

getCount :: String -> Int
getCount = length . filter (`elem` "aeiou") . map toLower

getCount :: String -> Int
getCount = length . flip intersect "aeiou"
