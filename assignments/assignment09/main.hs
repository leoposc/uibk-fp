{-
because of exercise 2 (the import of LeanCheck), start ghci via stack as follows:

stack ghci template_09.hs

(or put the Haskell code for exercise 2 into comments)
-}

-- Exercise 2

import Test.LeanCheck
import TreeModule

instance Listable a => Listable (Tree a) where
  tiers = cons3 Node \/ cons0 X

-- declare EQ instances for Tree a
instance Eq a => Eq (Tree a) where
    X == X = True
    (Node a l r) == (Node a' l' r') = a == a' && l == l' && r == r'
    _ == _ = False

prop_splitAtLevel_implies_fillXs :: 
    Int -> Tree Int -> Tree Int -> [Tree Int] -> Bool
prop_splitAtLevel_implies_fillXs i t s ss =
    splitResult ==> fillResult where
        splitResult = splitTree == s && splitList == ss where
            (splitTree, splitList) = splitAtLevel i t
        fillResult = fillTree == t && null fillList where
            (fillTree, fillList) = fillXs s ss
            

main :: IO ()
main = do
    check (prop_splitAtLevel_implies_fillXs :: Int -> Tree Int -> Tree Int -> [Tree Int] -> Bool)

-- Exercise 1

radius = 10  -- global radius

computeVolume :: Double -> Double
computeVolume rad = (4/3)*pi*rad^3

operationA :: Double -> Double
operationA radius =  computeVolume radius

operationB :: Double
operationB  = computeVolume radius

operationC :: Double -> Double
operationC = computeVolume

{-
Looking at the type signatures, we can see that operationA and operationC are the same. They both take a Double and return a Double. 
Which means they are not using the globally defined radius variable.

OperationB is different, because it does not take any arguments. It is a constant function, which always returns the same value and 
uses the globally defined radius variable.
-}


{- The following function works as expected. In the following variables which are refering
to each other will be renamed, so they have the same name. Makes it a little easier to read,
instead of incrementing each occurence and stating in a comment which var refers to which. -}


reverseList :: [a] -> [a]
reverseList xs_1 =
    let reverseListAux xs_2 ys_1 = case xs_2 of 
            (x:xs_3) -> reverseListAux xs_3 (x:ys_1)
            _ -> ys_1
    in reverseListAux xs_1 []


squareRootTwo :: Double -> Integer -> Double
squareRootTwo guess n
    | n == 0 = guess
    | otherwise = squareRootTwo ((guess + 2/guess) / 2) (n-1)

squareRootTwoA :: Double -> Integer -> Double
squareRootTwoA guess n
    | n == 0 = guess
    | otherwise = squareRootTwoA ((guess + 2/guess) / 2) (n-1) where n=n

squareRootTwoB :: Double -> Integer -> Double
squareRootTwoB guess n
    | n == 0 = guess
    | otherwise = let n_2 = n-1 in squareRootTwoB ((guess + 2/guess) / 2) n_2

{- 

a)

The following function does not work as expected. The problem is that we are trying to sign the global defined variable n to a new variable n, 
but we can not refer to a global variable in the scope where we defined a local variable with the same name. This applies to both functions.

A difference between function A and B is, that the local defined n "shadows" the global defined n in both patterns. While in function B,
the global defined n is still accessible in the "n == 0" case.

I modfied the squareRootTwoB function, so that it works as expected. Still there is no reason why this would be a good implementation. Since the first
function is more simple and readable, yet it does the same. 


b)

It is usually not good pratice to have global and local var/ functions of the same name,
because it can lead to confusion. For the compiler it is not a problem, because it is defined where to lookup first. 
So it is not leading to undefined behaviour, still the readability of the code is reduced significantly. -}