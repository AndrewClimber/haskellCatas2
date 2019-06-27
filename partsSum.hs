{-
Let us consider this example (array written in general format):

ls = [0, 1, 3, 6, 10]

Its following parts:

ls = [0, 1, 3, 6, 10]
ls = [1, 3, 6, 10]
ls = [3, 6, 10]
ls = [6, 10]
ls = [10]
ls = []

The corresponding sums are (put together in a list): [20, 20, 19, 16, 10, 0]

The function parts_sums (or its variants in other languages) will take as parameter a list ls and return a list of the sums of its parts as defined above.

Other Examples:
ls = [1, 2, 3, 4, 5, 6] 
parts_sums(ls) -> [21, 20, 18, 15, 11, 6, 0]

ls = [744125, 935, 407, 454, 430, 90, 144, 6710213, 889, 810, 2579358]
parts_sums(ls) -> [10037855, 9293730, 9292795, 9292388, 9291934, 9291504, 9291414, 9291270, 2581057, 2580168, 2579358, 0]
Notes
Some lists can be long.
Please ask before translating: some translations are already written and published when/if the kata is approved.

-}
module Solution where
import Data.List  

-- мои не прошло по скорости
partsSum :: [Integer] -> [Integer]
partsSum  =  map  sum . tls where 
    tls :: [a] -> [[a]]
    tls [] = [[]]
    tls y@(x:xs) = y:(tls xs)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' y@(x:xs) = y:(tails' xs)

tails'' :: [a] -> [[a]]
tails'' [] = [[]]
tails'' xs = xs : (tails'' $ tail xs)

partsSum' :: [Integer] -> [Integer]
partsSum' s =  map  sum $ tails'' s where 
    tails'' :: [a] -> [[a]]
    tails'' [] = [[]]
    tails'' xs = xs : (tails'' $ tail xs)


partsSum'' s  = [sum x | x <- (tails s)]


partsSum1 s = ps (tails s) where
    ps [] = []
    ps (x:xs) = sum(x):(ps xs)

partsSum2 s = ps (tails s) 0 where
    ps [] _ = []
    ps s n = (sum (head s)):( ps (drop (n+1) s) n )

partsSum3 s = ps s 0 where
    ps [] _ = [0]
    ps s n = (sum s):( ps (drop (n+1) s) n )    

partsSum4 s = ps s where
    ps [] = [0]
    ps (x:xs) = (sum (x:xs)):( ps xs )        

partsSum5 [] = [0]
partsSum5 (x:xs) = (sum (x:xs)):( partsSum5 xs )

partsSum6  = map (foldl (\acc x -> acc + x) 0 ) . tails

partsSum6'  = map (foldr (\x acc -> acc + x) 0 ) . tails

partsSum7  = map sum' . tails where
    sum' = foldl (\acc x -> acc + x) 0


-- моё прошло по скорости
partsSum = head . map (scanr (+) 0) . tails

partsSum9 = scanr (+) 0

-- codewars
partsSum :: [Integer] -> [Integer]
partsSum [] = [0]
partsSum (x : xs) = x + head t : t
  where t = partsSum xs
  
partsSum :: [Integer] -> [Integer]
partsSum = foldl (\acc n -> (head acc + n):acc) [0] . reverse

import Data.List (mapAccumR)

partsSum :: [Integer] -> [Integer]
partsSum = (\ a -> fst a : snd a ) . mapAccumR (\x y -> (x+y, x)) 0

