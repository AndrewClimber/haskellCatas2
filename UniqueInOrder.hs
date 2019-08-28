{-6 kyu
Unique In Order

Implement the function unique_in_order which takes as argument a sequence and returns a list of items without any elements with the same value next to each other and preserving the original order of elements.

For example:

uniqueInOrder "AAAABBBCCDAABBB" == "ABCDAB"
uniqueInOrder "ABBCcAD"         == "ABCcAD"
uniqueInOrder [1,2,2,3,3]       == [1,2,3]
-}

module UniqueInOrder (uniqueInOrder) where
import Data.List

-- aka
uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder s = concat $ map nub (group s)

-- codewars
uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder = map head . group

