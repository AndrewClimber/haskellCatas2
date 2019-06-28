{-
A child is playing with a ball on the nth floor of a tall building. The height of this floor, h, is known.

He drops the ball out of the window. The ball bounces (for example), to two-thirds of its height (a bounce of 0.66).

His mother looks out of a window 1.5 meters from the ground.

How many times will the mother see the ball pass in front of her window (including when it's falling and bouncing?

Three conditions must be met for a valid experiment:
Float parameter "h" in meters must be greater than 0
Float parameter "bounce" must be greater than 0 and less than 1
Float parameter "window" must be less than h.
If all three conditions above are fulfilled, return a positive integer, otherwise return -1.

Note:
The ball can only be seen if the height of the rebounding ball is stricty greater than the window parameter.

Example:
- h = 3, bounce = 0.66, window = 1.5, result is 3

- h = 3, bounce = 1, window = 1.5, result is -1 

(Condition 2) not fulfilled).

-}

module Codewars.Kata.BouncingBall where

-- моё
bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h b w | h <= 0.0 = (-1)
                   | b <= 0.0 || b >= 1.0 = (-1)
                   | w >= h = (-1)
                   | otherwise = calcNTimes (h*b) b w  0 
calcNTimes :: Double -> Double -> Double -> Integer -> Integer
calcNTimes acctimes b w ntimes  | acctimes > w = calcNTimes (acctimes*b) b w (ntimes+1)
                            | otherwise = (ntimes*2)+1
                            
-- codewars
bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h b w 
  | h > 0 && 0 < b && b < 1 && w < h = 2 + (bouncingBall (h * b) b w)
  | otherwise = -1