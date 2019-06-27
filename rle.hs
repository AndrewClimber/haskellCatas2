{-
Run-length encoding (RLE) is a very simple form of lossless data compression in which runs of data are stored as a single data value and count.

A simple form of RLE would encode the string "AAABBBCCCD" as "3A3B3C1D" meaning, first there are 3 A, then 3 B, then 3 C and last there is 1 D.

Your task is to write a RLE encoder and decoder using this technique. The texts to encode will always consist of only uppercase characters, no numbers.
-}
module Kata.RLE (encode,decode) where
import Data.List
import Data.Char

-- моё
encode :: String -> String              
myFilter = filter (`notElem` ['[',']','(',')',',','\'','\"'])
encode str = myFilter $ show $ zip (map length (group str)) (map head (group str))
{-
Составные части для колбасы.
en str = map length (group str)
en1 str = map head (group str)
eenn str = filter(/=')') (filter(/='(') (filter(/='[') (filter(/=']') (filter(/='\'') (filter(/=',') (show (zip (en str) (en1 str))))))))
-}
decode :: String -> String          
decode str = myFilter $ show $ dec str where
        dec [] = []
        dec str = (replicate ( read rn::Int ) (str !! lenNum )):(dec (drop (lenNum+1) str)) where
                lenNum = length rn
                rn@readNumber = takeWhile isDigit str          




{-
            -- codewars            
import Data.List (group,groupBy)
import Data.Function (on)
import Data.Char (isDigit)
            
encode :: String -> String
encode = concatMap ( \ xs -> show (length xs) ++ [head xs] ) . group
            
decode :: String -> String
decode input = go (groupBy ((==) `on` isDigit) input) where
        go :: [String] -> String
        go [] = ""
        go (x:y:zs) = replicate (read x) (head y) ++ go zs            

-----------------------------------------------------------------------------
import Data.Char
import Data.Function
import Data.List

encode :: String -> String
encode = concatMap (\g@(x:_) -> show (length g) ++ [x]) . group

decode :: String -> String
decode = run . groupBy ((&&) `on` isDigit) where
    run [] = []
    run (len:[char]:xs) = replicate (read len) char ++ run xs
-}    