{-
Write a function that takes a positive integer and returns the next smaller positive integer containing the same digits.

For example:

nextSmaller(21) == 12
nextSmaller(531) == 513
nextSmaller(2071) == 2017
Return -1 (for Haskell: return Nothing), when there is no smaller number that contains the same digits. 
Also return -1 when the next smaller number with the same digits would require the leading digit to be zero.

nextSmaller(9) == Nothing
nextSmaller(135) == Nothing
nextSmaller(1027) == Nothing -- 0721 is out since we don't write numbers with leading zeros
some tests will include very large numbers.
test data only employs positive integers.
The function you write for this challenge is the inverse of this kata: "Next bigger number with the same digits."
-}
module NextSmaller where 

import Data.List (delete, sortBy)

permutations :: Eq a => [a] -> [[a]]
permutations []   = [[]]
permutations list = [(x:xs) | x <- list, xs <- permutations $ delete x list]

    

--nextSmaller :: Integer -> Maybe Integer
nextSmaller nuM =  head (sortBy (compare) $ map read (permutations $ show nuM) :: [Integer]  where
    permutations []   = [[]]
    permutations list = [ (x:xs) | x <- list, xs <- permutations $ delete x list]


--resNu = map nextSmaller     
