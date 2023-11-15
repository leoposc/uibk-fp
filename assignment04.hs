data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving (Show)

exampleTree :: Tree Float
exampleTree = Node 1 (Leaf 2) (Node 3 (Node 4 (Leaf 5) (Leaf 6)) (Leaf 7))

-- Question 1
height :: Tree a -> Integer
height (Leaf x) = 0
height (Node x t1 t2) = 1 + max (height t1) (height t2)

-- Question 2
flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node a l r) = flatten l ++ [a] ++ flatten r

-- Question 3
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = isStrictlySorted . flatten

isStrictlySorted :: Ord a => [a] -> Bool
isStrictlySorted [] = True
isStrictlySorted [x] = True
isStrictlySorted (x : y : xs) = x < y && isStrictlySorted (y : xs)


-- Question 4
-- function return the depth of the element in the tree or Nothing if the element is not in the tree
elemDepth :: Eq a => a -> Tree a -> Maybe Integer
elemDepth x tree = searchTree x tree 0

searchTree :: Eq a => a -> Tree a -> Integer -> Maybe Integer
searchTree x (Leaf a) depth = if x == a then Just depth else Nothing
searchTree x (Node a l r) depth = if x == a then Just depth else searchTree x l (depth + 1) `orElse` searchTree x r (depth + 1)
-- searchTree x (Node a l r) depth = if x == a then Just depth else orElse (searchTree x l (depth + 1)) (searchTree x r (depth + 1))

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing x = x
orElse x _ = x





----------------------------------------
-- Tests -------------------------------
-- Run testAll to test your functions --
----------------------------------------

testAll = tests1 >> tests2 >> tests3 >> tests4

tests1 =
  checkNumTrees "Ex 1 test 1" [3, 0, 2, 2] (height . fst)
    >> checkCharTrees "Ex 1 test 2" [3] (height . fst)

tests2 =
  checkNumTrees "Ex 2 test 1" [[2, 1, 5, 4, 6, 3, 7], [1], [-3, -2, 0, 5, 7], [-1.5, -1, -0.5, 0, 1, 1.5, 1.5]] (flatten . fst)
    >> checkCharTrees "Ex 2 test 2" [['b', 'a', 'b', 'a', 'a', 'b', 'b']] (flatten . fst)

tests3 =
  checkNumTrees "Ex 3 test 1" [False, True, True, False] (isSearchTree . fst)
    >> checkCharTrees "Ex 3 test 2" [False] (isSearchTree . fst)

tests4 =
  checkNumTrees "Ex 4 test 1" [Nothing, Nothing, Just 2, Just 0] (elemDepth 0 . fst)
    >> checkCharTrees "Ex 4 test 2" [Just 1] (elemDepth 'b' . fst)

-- internal construction of tests
exT1 = Leaf 1

exT2 = Node (-2) (Leaf (-3)) (Node 5 (Leaf 0) (Leaf 7))

exT3 = Node 0 (Node (-1) (Leaf (-1.5)) (Leaf (-0.5))) (Node 1.5 (Leaf 1) (Leaf 1.5))

exT4 = Node 'a' (Leaf 'b') (Node 'a' (Leaf 'b') (Node 'b' (Leaf 'a') (Leaf 'b')))

checkNumTrees :: (Show a, Show b) => String -> [b] -> ((Tree Float, b) -> a) -> IO ()
checkNumTrees name xs =
  checkAll name (zip [exampleTree, exT1, exT2, exT3] xs) (show . snd) (show . fst)

checkCharTrees :: (Show a, Show b) => String -> [b] -> ((Tree Char, b) -> a) -> IO ()
checkCharTrees name xs =
  checkAll name [(exT4, head xs)] (show . snd) (show . fst)

checkAll ::
  Show a =>
  String ->
  [b] ->
  (b -> String) ->
  (b -> String) ->
  (b -> a) ->
  IO ()
checkAll name xs e err c = do
  putStr ("*** " ++ name ++ ": ")
  let errors = filter (\x -> show (c x) /= e x) xs
  if null errors
    then putStrLn "OK"
    else do
      let x = head errors
      putStrLn
        ( "ERROR; expected '"
            ++ e x
            ++ "', but found '"
            ++ show (c x)
            ++ "' for value "
            ++ err x
        )