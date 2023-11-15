test = [1,2,3]

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs


sSum :: Num a => [a] -> [a]
sSum [] = []
sSum (x:xs) = mySum (x:xs) : sSum xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

replace :: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x: xs) a b 
  | x == a = b : replace xs a b
  | otherwise = x : replace xs a b