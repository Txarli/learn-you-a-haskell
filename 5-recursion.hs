maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

maximumWithMax :: (Ord a) => [a] -> a
maximumWithMax [] = error "maximum of empty list"
maximumWithMax [x] = x
maximumWithMax (x:xs) = max x (maximumWithMax xs)

-- replicate' 3 5 = [5, 5, 5]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n - 1) x


-- take' 3 [1, 2, 3, 4, 5] = [1, 2 ,3]
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: (Ord i) => [i] -> [i]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]  
repeat' x = x:repeat' x  

-- zip' [1,2,3] [2,3] = [(1, 2), (2, 3)]
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b):zip' as bs

-- elem' 3 [1, 2, 3] = True
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
