isTriangle :: Integer->Integer->Integer->Bool
isTriangle a b c = 
    if c - (a + b) < 0 && a - (b + c) < 0 && b - (a + c) < 0
        then True
        else False
