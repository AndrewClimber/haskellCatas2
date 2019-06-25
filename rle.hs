{-
Run-length encoding (RLE) is a very simple form of lossless data compression in which runs of data are stored as a single data value and count.

A simple form of RLE would encode the string "AAABBBCCCD" as "3A3B3C1D" meaning, first there are 3 A, then 3 B, then 3 C and last there is 1 D.

Your task is to write a RLE encoder and decoder using this technique. The texts to encode will always consist of only uppercase characters, no numbers.
-}
module Kata.RLE (encode,decode) where
import Data.List
import Data.Char


encode :: String -> String
encode str = 
     filter(/=',') (filter(/='[') (filter(/=']') (filter (/='\"') 
     (show ((transpose [((filter (/='[') (filter (/=']') (filter(/=',') 
     (show (map length (group str))))))),(map head (group str))]))))))

encode' str = 
     fi1 (fi2 (fi3 (fi4 (show ((transpose [((fi2 (fi3 (fi1 
     (show (map length (group str))))))),(map head (group str))]))))))  where
        fi1 = filter(/=',')
        fi2 = filter(/='[')
        fi3 = filter(/=']')
        fi4 = filter (/='\"')

en str = map length (group str)

en1 str = map head (group str)

en2 str = show (en str)

en3 str = (filter (/='[') (filter (/=']') (filter(/=',') (show (map length (group str))))))

en4 str = (transpose [(en3 str),(map head (group str))])

en8 str = filter(/='[') (filter(/=']') (filter(/='\"') (filter(/=',') (show (en4 str)))))

--decode :: String -> String
decode [] = []
decode (x:xs) = (replicate ( read readNumber::Int ) (xs !! (lenNum-1) )) :(decode (drop (lenNum) xs)) where
    lenNum = length readNumber
    readNumber = (takeWhile (isDigit) (x:xs))

decode' str = filter(/='[') (filter(/=']') (filter(/='\"') (filter(/=',') (show (dec str))))) where
    dec [] = []
    dec (x:xs) = (replicate ( read readNumber::Int ) (xs !! (lenNum-1) )) :(dec (drop (lenNum) xs)) where
        lenNum = length readNumber
        readNumber = (takeWhile (isDigit) (x:xs))

decode'' str = filter(/='[') (filter(/=']') (filter(/='\"') (filter(/=',') (show (dec str))))) where
        dec [] = []
        dec str = (replicate ( read readNumber::Int ) (str !! (lenNum) )) :(dec (drop (lenNum+1) str)) where
            lenNum = length readNumber
            readNumber = (takeWhile (isDigit) str)        

