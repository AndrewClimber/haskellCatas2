{- 7 kyu
Remove duplicate words
Your task is to remove all duplicate words from a string, leaving only single (first) words entries.

Example:

Input:

'alpha beta beta gamma gamma gamma delta alpha beta beta gamma gamma gamma delta'

Output:

'alpha beta gamma delta'

-}

module RemoveDuplicateWords where 
import Data.List

removeDuplicateWords :: String -> String
removeDuplicateWords  =  unwords . nub . words
