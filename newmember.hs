{-
The Western Suburbs Croquet Club has two categories of membership, Senior and Open. They would like your help 
with an application form that will tell prospective members which category they will be placed.

To be a senior, a member must be at least 55 years old and have a handicap greater than 7. In this croquet club, 
handicaps range from -2 to +26; the better the player the lower the handicap.

Input
Input will consist of a list of lists containing two items each. Each list contains information for a single potential member. 
Information consists of an integer for the person's age and an integer for the person's handicap.

Example Input
[(18, 20),(45, 2),(61, 12),(37, 6),(21, 21),(78, 9)]
Output
Output will consist of a list of string values (in Haskell: Open or Senior) stating whether the respective member 
is to be placed in the senior or open category.

Example Output
[Open, Open, Senior, Open, Open, Senior]

openOrSenior []        `shouldBe` []
openOrSenior [(18,20)] `shouldBe` [Open]
openOrSenior [(55,20)] `shouldBe` [Senior]
openOrSenior [(55,7)] `shouldBe` [Open]
openOrSenior [(55,7),(55,8),(54,9)] `shouldBe` [Open, Senior, Open]
-}

data Membership = Open | Senior deriving (Eq, Show)

-- моё
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior [] = []
openOrSenior (x:xs) = (os x): openOrSenior xs where 
    os str | (fst str) >= 55 && (snd str) > 7 = Senior
           | otherwise = Open
    


-- codewars
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior l = [ if a>=55 && h>7 then Senior else Open | (a, h) <- l]

openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior = map (\(age,hc) -> if age >= 55 && hc > 7 then Senior else Open)



