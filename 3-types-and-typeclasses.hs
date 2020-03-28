removeNonUppercase :: String -> String  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z

{-
    Int: Integer
    Integer: Integer not bounded

    Float: Real floating point with single precission
    Double: Real floating point with double de precision

    Bool: Boolean -> True False

    Char: A character -> ''
-}
