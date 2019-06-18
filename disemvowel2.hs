module Disemvowel where
    disemvowel :: String -> String
    disemvowel str = [c | c <- str, c `notElem` ['a','e','i','o','u','A','E','I','O','U'] ]
    