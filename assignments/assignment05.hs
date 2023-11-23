import GHC.Arr (fill)
{- Exercise 1 -}

{-
     Difference between type and data:
      Type is used to create type synonyms, which are used to make code more readable.

      Data is used to create completely new data types with constructors. They can hold
      distinct values and structures. They can also be recursive, polymorhic and are 
      often used for pattern matching.
-}
type Age = (String, Integer)

exampleAges :: [Age]
exampleAges = [("Alice", 17), ("Bob", 35), ("Clara", 17), ("Hanna", 17)]

-- Question 1.2
ticketCostA :: Age -> String
ticketCostA (name, age) = let
    cost = if age < 0 then error "Age cannot be negative" else if age < 13 then "5" else if age < 18 then "7.50" else "15"
    formatCost = name ++ " pays " ++ cost ++ " euros for a ticket"
  in formatCost

ticketCostB :: Age -> String
ticketCostB (name, age) = name ++ " pays " ++ x ++ " euros for a ticket"
  where x | age < 0 = error "Age cannot be negative"
          | age < 13 = "5"
          | age < 18 = "7.50"
          | age > 17 = "15"


-- ticketCostC :: Age -> String
-- ticketCostC (name, age) = case age of
--   age | age < 0 -> error "Age cannot be negative"
--       | age < 13 -> formatCost (name ++ " 5")
--       | age < 18 -> formatCost (name ++ " 7.50")
--       | otherwise -> formatCost (name ++ " 15")
--     where formatCost = name ++ " pays " ++ show cost ++ " euros for a ticket."
--             cost = if age < 0 then error "Age cannot be negative" else if age < 13 then 5 else if age < 18 then 7.50 else 15

-- Question 1.3
ageLookup :: [Age] -> Integer -> Maybe [String]
ageLookup [] _ = Nothing
ageLookup ((name, a):xs) age = case age of
  age | age == a -> case ageLookup xs age of
        Just names -> Just (name : names) -- Add the name to the list if age matches.
        Nothing    -> Just [name]         -- Wrap the name in Just if there were no previous matches.
      | otherwise -> ageLookup xs age



-- Question 1.4
bidirectionalLookup :: (Eq b, Eq a) => Either a b -> [(a, b)] -> Maybe (Either a b)
bidirectionalLookup _ [] = Nothing
bidirectionalLookup (Left a) ((a', b):xs) = if a == a' then Just (Right b) else bidirectionalLookup (Left a) xs
bidirectionalLookup (Right b) ((a, b'):xs) = if b == b' then Just (Left a) else bidirectionalLookup (Right b) xs

{- Exercise 2 -}
data Tree a = Node a (Tree a) (Tree a) | X deriving Show

exampleTree = Node 1 (Node 2 X X) (Node 3 X (Node 4 X X))

takeLevels :: Int -> Tree a -> Tree a
takeLevels 0 _ = X
takeLevels _ X = X
takeLevels n (Node a l r) = Node a (takeLevels (n - 1) l) (takeLevels (n - 1) r)

dropLevels :: Int -> Tree a -> [Tree a]
dropLevels _ X = []
dropLevels 0 t = [t]
dropLevels n (Node a l r) = X : dropLevels (n - 1) l ++ dropLevels (n - 1) r

splitAtLevel :: Int -> Tree a -> (Tree a, [Tree a])
splitAtLevel 0 _ = (X, [])
splitAtLevel _ X = (X, [])
splitAtLevel n (Node a l r) = (Node a (takeLevels (n - 1) l) (takeLevels (n - 1) r), X : dropLevels (n - 1) l ++ dropLevels (n - 1) r)

fillXs :: Tree a -> [Tree a] -> (Tree a, [Tree a])
fillXs X (t: ts) = (t, ts)
fillXs (Node a l r) ts = 
    let
        (l', ts') = fillXs l ts
        (r', ts'') = fillXs r ts'
    in (Node a l' r', ts'')


{- Tests -}
checkEx1 = sequence_ [test1, test2, test3, test4]

checkEx2 = mapM_ putStrLn  [test5, test6, test7, test8, test9, test10]

-- Internal construction of tests
test1 = mapM_ (putStrLn . (\((n, a), c) -> check "ticketCostA" (show $ n ++ " pays " ++ c ++ " euros for a ticket") (ticketCostA (n, a)))) (zip testAges testCosts)

test2 = mapM_ (putStrLn . (\((n, a), c) -> check "ticketCostB" (show $ n ++ " pays " ++ c ++ " euros for a ticket") (ticketCostB (n, a)))) (zip testAges testCosts)

test3 =
  mapM_
    (putStrLn . (\(i, o) -> check "ageLookup" (show o) (ageLookup testAges i)))
    (zip [50, 13, 12] [Just ["D"], Nothing, Just ["B", "E"]])

test4 =
  mapM_
    (putStrLn . (\(i, o) -> check "bidirectionalLookup" (show o) (bidirectionalLookup i testAges)))
    (zip [Right 0, Right 12, Left "E", Left "F"] [Just (Left "A"), Just (Left "B"), Just (Right 12), Nothing])

test5 = check "test1" (show $ Node 1 (Node 2 X X) (Node 3 X X)) (takeLevels 2 exampleTree)
test6 = check "test2" (show [X, X, X, Node 4 X X]) (dropLevels 2 exampleTree)
test7 = check "test3" (show (Node 1 (Node 2 X X) (Node 3 X X), [X, X, X, Node 4 X X])) (splitAtLevel 2 exampleTree)
test8 = check "test4" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 0 exampleTree)
test9 = check "test5" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 2 exampleTree)
test10 = check "test6" (show (exampleTree, [] :: [Tree Int])) (uncurry fillXs $ splitAtLevel 5 exampleTree)

testAges = [("A", 0), ("B", 12), ("C", 17), ("D", 50), ("E", 12)]
testCosts = ["5", "5", "7.50", "15", "5"]

check name e c =
  "*** " ++ name ++ ": " ++ (
    if show c == e then "OK"
    else "ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")