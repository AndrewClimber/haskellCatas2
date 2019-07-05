{- 5 kyu . Last digit of a large number
Define a function that takes in two non-negative integers a and b and returns the last decimal digit of a^b. 
Note that a and b may be very large!

For example, the last decimal digit of 9^7 is 9, since 9^7 = 4782969. The last decimal digit of (2^200)^(2^300), 
which has over 10^92 decimal digits, is 6. Also, please take 0^0 to be 1.

You may assume that the input will always be valid.

Examples
lastDigit 4 1             `shouldBe` 4
lastDigit 4 2             `shouldBe` 6
lastDigit 9 7             `shouldBe` 9
lastDigit 10 (10^10)      `shouldBe` 0
lastDigit (2^200) (2^300) `shouldBe` 6
-}
{-# LANGUAGE MultiWayIf #-}
module LastDigit where
import Data.Bits
--- aka
-- hint here : https://school-science.ru/2/7/30466
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit a b =  
    case lastNum a of
         0 -> 0
         1 -> 1
         5 -> 5
         6 -> 6
         _ -> anotherNum a b
    where
        anotherNum a b = 
            if | (even a) && (b `mod` 4 == 0)  -> 6
               | (not $ even a) && (b `mod` 4 == 0) -> 1
               | (b `mod` 4 == 1) -> lastNum a
               | (b `mod` 4 == 2) -> lastNum $ lastNum a^2
               | (b `mod` 4 == 3) -> lastNum $ lastNum a^3
        lastNum n = read [last $ show (n)]::Integer


-- codewars
lastDigit :: Integer -> Integer -> Integer
lastDigit a b = ((a `rem` 10) ^ ((b - 1) `rem` 4 + 1)) `rem` 10

---
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit a b = mod ((mod a 10) ^ bRes) 10
  where bRes = let bMod = mod b 400 in if bMod == 0 then 4 else bMod

---

lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1 
lastDigit a b = (a `mod` 10) ^ ((b - 1) `mod` 4 + 1) `mod` 10  

-------------------
lastDigit :: Integer -> Integer -> Integer
lastDigit a 0 = 1
lastDigit a b = ds !! fromInteger ((b-1) `mod` toInteger (length ds))
  where ds = powdig !! fromInteger (a `mod` 10)

powdig :: [[Integer]]
powdig = [[0],[1],[2,4,8,6],[3,9,7,1],[4,6],[5],[6],[7,9,3,1],[8,4,2,6],[9,1]]
-----------------------------

-- instead of 'b1 = y mod 4' in [0..3] I do 'b2 = y mod 4' in [4..7],
-- and instead of a^b1 I do multiply a with itself exaclty b2 times, which is faster
-- for example, 7*7*7*7*7*7*7 is faster than 7^3
-- but perhaps this is more readable: lastDigit x y = a^b `mod` 10

lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit x y = iterate (*a) 1 !! fromIntegral b `mod` 10
  where a = x `mod` 10
        b = 4 + y `mod` 4
-------------------------
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit a b = case mod a 10 of
    0 -> 0
    1 -> 1
    2 -> case mod b 4 of
      0 -> 6
      1 -> 2
      2 -> 4
      3 -> 8
    3 -> case mod b 4 of
      0 -> 1
      1 -> 3
      2 -> 9
      3 -> 7
    4 -> case mod b 2 of
      0 -> 6
      1 -> 4
    5 -> 5
    6 -> 6
    7 -> case mod b 4 of
      0 -> 1
      1 -> 7
      2 -> 9
      3 -> 3
    8 -> case mod b 4 of
      0 -> 6
      1 -> 8
      2 -> 4
      3 -> 2
    9 -> case mod b 2 of
      0 -> 1
      1 -> 9
------------------------------
lastDigit :: Integer -> Integer -> Integer
lastDigit _ 0 = 1
lastDigit b e = lst !! ix
    where lst = map (`rem` 10) $ zipWith (^) (repeat b) [4..7]
          ix  = (fromIntegral $ e `rem` 4)


