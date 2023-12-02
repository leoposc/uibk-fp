myAbs :: (Num a) => (a -> a) -> a -> a
myAbs f = abs . f
-- myAbs f x = abs (f x)

g x = x^2-4


div2 :: [Integer] -> [Integer]
div2 = map (`div` 2) . filter even