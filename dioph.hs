{-
5 kuy. Diophantine Equation
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
    
-- aka    
{-
(x-2*y)*(x+2*y) = n  , (x-2*y)=a, (x+2*y)=b , a*b=n => для нахождения корней уравнения можно использовать перебор не всех чисел, а
перебор всех делителей числа.  k1,k2 - делители.
x-2*y = k1, x+2*y = k2
x = (k1+k2)/2 , y = (k2-k1)/4
-}
solequa :: Integer -> [(Integer, Integer)]
solequa n  = 
    [((x k1 k2),(y k1 k2)) | k1 <- (divs n), k2 <- (divs n), k2 >= k1, (equation k1 k2) == n ] where
        x k1 k2 = (k2+k1) `div` 2
        y k1 k2 = (k2-k1) `div` 4
        equation k1 k2 = ((x k1 k2) - 2*(y k1 k2)) * ((x k1 k2) + 2*(y k1 k2))
        divs n = let 
            r = floor $ sqrt $ fromIntegral n 
            (a,b) = unzip $ (1,n) : [(d, q) | d<-[2..r-1], let (q,r)=quotRem n d, r==0] in
                   if r*r==n  then a ++ r : reverse b  else a ++ reverse b
            
{-
вычисление делителей

-- этот вычисляет правильно. но используя его - не укладывался по времени
divisors n = [x | x <- [1..n], n `rem` x == 0]

-- этот вычисляет делители неправильно. но с ним я уложился по времени
divs n = 
        let 
          r = floor $ sqrt $ fromIntegral n 
          (a,b) = unzip $ (1,n) : [(d, q) | d <- [2..r-1], let (q,r)=quotRem n d, r==0]
        in
          if r*r==n
            then a ++ r : reverse b
            else a ++ reverse b
-}                

-- codewars
solequa' :: Integer -> [(Integer, Integer)]
solequa' n = [(div (p + q) 2, div (q - p) 4 ) | i <- [1..truncate (sqrt (fromIntegral n))], let (p, q) = (i, div n i), ((mod n i) == 0) && (mod (p + q) 2 == 0) && (mod (q - p) 4 == 0)]

solequa'' :: Integer -> [(Integer, Integer)]
solequa'' n = map (\(b, a) -> (a + (b-a) `div` 2, (b-a) `div` 4)) . filter ((==0) . (`mod` 4) . uncurry (-)) . map (\d -> (n `div` d, d)) $ k
    where k = filter (\d -> n `mod` d == 0) [1.. floor . sqrt . fromIntegral $ n]


solequa''' :: Integer -> [(Integer, Integer)]    
solequa''' n = concat $ check <$> [1..round (sqrt $ fromIntegral n) + 1]
    where check i = if n `mod` i == 0 && ((i + j) `mod` 2 == 0) && ((j - i) `mod` 4 == 0)
                        then [(x, y)]
                        else []
                            where j = n `div` i
                                  x = (i + j) `div` 2
                                  y = (j - i) `div` 4

solequa'''' :: Integer -> [(Integer, Integer)]
solequa'''' n = filter (\(p,q) -> p^2 - 4 * q^2 == n) $ map (\(x,y) -> (div (x+y) 2, div (y-x) 4)) [(a,div n a) | a <- [1..(round . sqrt . fromIntegral) n], mod n a == 0]



divisors n = init [x | x <- [1..n], n `rem` x == 0]