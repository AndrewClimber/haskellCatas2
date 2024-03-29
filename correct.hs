{-
Character recognition software is widely used to digitise printed texts. Thus the texts can be edited, searched and stored on a computer.

When documents (especially pretty old ones written with a typewriter), are digitised character recognition softwares often make mistakes.

Your task is correct the errors in the digitised text. You only have to handle the following mistakes:

S is misinterpreted as 5
O is misinterpreted as 0
I is misinterpreted as 1
The test cases contain numbers only by mistake.
-}
module CodeWars.OCRMistakes where

-- моё    
correct :: String -> String
correct xs = [if x == '5' then 'S' else if x == '0' then 'O' else if x == '1' then 'I' else x  | x <- xs ]

-- codewars
correct' = map $ \case '0' -> 'O'; '1' -> 'I'; '5' -> 'S'; x -> x

correct'' = map (\el -> case el of '5' -> 'S'; '1' -> 'I'; '0' -> 'O'; el -> el)