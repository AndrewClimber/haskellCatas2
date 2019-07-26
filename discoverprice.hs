{- 7 kyu
Discover The Original Price
We need to write some code to return the original price of a product, the return type must be of type decimal and the number must 
be rounded to two decimal places.

We will be given the sale price (discounted price), and the sale percentage, our job is to figure out the original price.

For example:
Given an item at $75 sale price after applying a 25% discount, the function should return the original price of that item before 
applying the sale percentage, which is ($100.00) of course, rounded to two decimal places.

DiscoverOriginalPrice(75, 25) => 100.00M where 75 is the sale price (discounted price), 25 is the sale percentage and 100 is 
the original price
-}

module Discover where
import Text.Printf
-- aka
discoverOriginalPrice :: Double -> Double -> Double
discoverOriginalPrice d p =  read (printf "%.2f" (d/(1 - p/100)))::Double

-- codewars
discoverOriginalPrice :: Double -> Double -> Double
discoverOriginalPrice sale discount = roundTo 2 $ sale * 100 / (100 - discount)
-- округление числа X до n знаков после запятой. Может пригодиться !!!
roundTo n x = (fromIntegral . round $ x * 10^n) / 10^n

