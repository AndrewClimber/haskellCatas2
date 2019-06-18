module Disemvowel where
    disemvowel :: String -> String
    disemvowel str = [c | c <- str, c `notElem` "aeiouAEIOU" ]
    