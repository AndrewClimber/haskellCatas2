import Data.List (genericReplicate)

convMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"),
       (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"),
       (4,"IV"), (1,"I")]

toRoman :: Integer -> String
toRoman 0 = "N"
toRoman x | x > 0 = snd$ foldl f (x,[]) convMap
   where f (n,s) (rn, rs) = (l, s ++ concat (genericReplicate k rs))
         where (k,l) = divMod n rn