{-6 kyu
Valid Braces
Write a function that takes a string of braces, and determines if the order of the braces is valid. 
    It should return true if the string is valid, and false if it's invalid.

This Kata is similar to the Valid Parentheses Kata, but introduces new characters: brackets [], and curly braces {}. 
Thanks to @arnedag for the idea!

All input strings will be nonempty, and will only consist of parentheses, brackets and curly braces: ()[]{}.

What is considered Valid?
A string of braces is considered valid if all braces are matched with the correct brace.

Examples
"(){}[]"   =>  True
"([{}])"   =>  True
"(}"       =>  False
"[(])"     =>  False
"[({})](]" =>  False

-}

module Codewars.Kata.Braces where

--validBraces :: String -> Bool
leftBraces = ['(','[','{']
rightBraces = [')',']','}']
pairsBraces = ["()","[]","{}"]
validBraces s = valid s where 
    valid [] = []
    valid (x1:x2:xs) | (x1 `elem` leftBraces) && (x2 `elem` leftBraces) = x1:x2:valid xs
                     | x1 `notElem` leftBraces = error "Wrong braces"
                     | x2 `notElem` leftBraces = if any (==x1:[x2]) pairsBraces then 


leftBC s = [x | x <- s, x `elem` leftBraces]

dropBC s = [x | x <- s, x `elem` pairsBraces]
