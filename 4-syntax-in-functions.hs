{--- Pattern matching ---}
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil" 
{-
    Fallaría porque no tiene non-exhaustive pattern, es decir, un catch-all pattern

    charName 't'
    "*** Exception: 4-syntax-in-functions.hs:(19,1)-(21,22): Non-exhaustive patterns in function charName
-}

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Podemos obviar por ejemplo en un tuple unos elementos con '_'
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

sumTuple :: (Num a) => (a, a) -> a
sumTuple (x, y) = x + y

sumDoubleTuples :: (Num a) => [(a, a)] -> [a]
sumDoubleTuples array = [x + y | (x, y) <- array]

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  

{--- Guards ---}
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  

myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"
    where 
        bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)  
-- el scope de where será la función, nadie más podrá acceder a esos valores

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
    
{--- let in ---}
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

{--- case of ---}
-- los matcher, en realidad, son syntax sugar para un case of
headCaseOf' :: [a] -> a  
headCaseOf' xs = case xs of [] -> error "No head for empty lists!"  
                            (x:_) -> x  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of  [] -> "empty."  
                                                [x] -> "a singleton list."  
                                                xs -> "a longer list." 

-- podríamos hacer también
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
