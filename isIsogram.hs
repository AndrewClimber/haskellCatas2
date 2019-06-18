import qualified Data.Set as Set
import Data.Char


isIsogram :: String -> Bool
isIsogram p | s1 == s2  = True
            | otherwise = False where 
                s1 =  length $ Set.fromList $ map (\x -> show [toUpper(x)]) p
                s2 =  length $ map (\x ->  show [toUpper(x)]) p


isIsogram' :: String -> Bool
isIsogram' p = length (Set.fromList $ map (\x -> show [toUpper(x)]) p) == length ( map (\x ->  show [toUpper(x)]) p)

isIsogram'' :: String -> Bool
isIsogram'' p = length (Set.fromList $ ma) == length ( ma ) where
    ma = map func p
    func = (\x -> show [toUpper(x)])

isIsogram''' :: String -> Bool
isIsogram''' p = length (Set.fromList $ ma) == length ( ma )  where
    ma = map func p
    func = \x -> [toUpper x]

isIsogram'''' :: String -> Bool
isIsogram'''' p = length (Set.fromList $ ma) == length ( ma )  where
    ma = map toUpper p

      