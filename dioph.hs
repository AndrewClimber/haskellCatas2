{-
In mathematics, a Diophantine equation is a polynomial equation, usually with two or more unknowns, 
such that only the integer solutions are sought or studied.

In this kata we want to find all integers x, y (x >= 0, y >= 0) solutions of a diophantine equation of the form:

x2 - 4 * y2 = n
(where the unknowns are x and y, and n is a given positive number) in decreasing order of the positive xi.

If there is no solution return [] or "[]" or "". (See "RUN SAMPLE TESTS" for examples of returns).

Examples:
solEquaStr(90005) --> "[[45003, 22501], [9003, 4499], [981, 467], [309, 37]]"
solEquaStr(90002) --> "[]"
Hint:
x2 - 4 * y2 = (x - 2*y) * (x + 2*y)
-}
module Codewars.Kata.Dioph where
import Data.List


solequa :: Integer -> [(Integer, Integer)]
solequa n = tail $ nub  [if (x - 2*y) * (x + 2*y) == n then (x,y) else (0,0)  | x <- [0..((n `div` 2)+10)], y <- [0..((n `div` 2)+10)]]

sol n  = tail $ nub  [if (x - 2*y) * (x + 2*y) == n then (x,y) else (0,0)  | x <- [0..100], y <- [0..100] ]

-- sol1 n = scanr (\x y ->if (x - 2*y) * (x + 2*y) == n then (x,y) else (0,0))  0
sol1 n  = tail $ nub  [if (x - 2*y) * (x + 2*y) == n then (x,y) else (0,0)  | x <- (deli n), y <- (deli n) ]

sol2 n  = tail $ nub  [if (x - 2*y) * (x + 2*y) == n then (x,y) else (0,0)  | x <- [0,1,5], y <- [0,1,5] ]


sol3 n = tail $ nub  [if x*y == n then (x,y) else (0,0)  | x <- (deli n), y <- (deli n) ]

deli n = sort $ nub [if snd (n `divMod` x) == 0 then fst (n `divMod` x) else 0 | x <- [1..n]]

