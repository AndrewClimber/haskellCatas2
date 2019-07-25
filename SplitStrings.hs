{- 6 kyu
Split Strings
Complete the solution so that it splits the string into pairs of two characters. 
If the string contains an odd number of characters then it should replace the missing second character 
of the final pair with an underscore ('_').

Examples:

solution "abc" `shouldBe` ["ab", "c_"]
solution "abcdef" `shouldBe` ["ab", "cd", "ef"]
-}
module Codewars.Kata.SplitStrings where

-- aka
solution :: String -> [String]
solution [] = []
solution [x1] = [[x1]++"_"]
solution (x1:x2:xs) = (x1:[x2]):solution xs

-- codewars
solution :: String -> [String]
solution [] = []
solution [c] = [c:"_"]
solution (a:b:t) = [a,b] : solution t

solution :: String -> [String]
solution []       = []
solution (x:[])   = (x:"_"):[]
solution (x:y:xs) = (x:y:[]):solution xs

