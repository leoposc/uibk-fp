-- The following imports are solely used for testing and time measurements
import System.CPUTime
import System.Timeout
import Text.Printf

{- Exercise 1 -}
type Vector a = [a]

type Matrix a = [[a]]

indices :: Vector a -> [Int]
indices v = [0 .. length v - 1]

-- Task 1.1
negateVecSlow :: Num a => Vector a -> Vector a
negateVecSlow v = [- v !! i | i <- indices v]

negateVec :: Num a => Vector a -> Vector a
negateVec = map negate

-- Task 1.2
vecAddSlow :: Num a => Vector a -> Vector a -> Vector a
vecAddSlow v w = [v !! i + w !! i | i <- indices v]

vecAdd :: Num a => Vector a -> Vector a -> Vector a
vecAdd = zipWith (+)

-- Task 1.3
scalarProductSlow :: Num a => Vector a -> Vector a -> a
scalarProductSlow v w = sum [v !! i * w !! i | i <- indices v]

scalarProduct :: Num a => Vector a -> Vector a -> a
scalarProduct v = sum . zipWith (*) v
-- Task 1.4
transposeSlow :: Matrix a -> Matrix a
transposeSlow a =
  let rowIdcs = indices a
      colIdcs = indices (head a)
   in [[a !! i !! j | i <- rowIdcs] | j <- colIdcs]

-- tranpose a Matrix
transpose :: Matrix a -> Matrix a
transpose [] = []
transpose ([]:_) = []
transpose rows = map head rows : transpose (map tail rows)


-- Task 1.5
matMultSlow :: Num a => Matrix a -> Matrix a -> Matrix a
matMultSlow a b =
  let n = indices a
      m = indices (head b)
   in [[scalarProductSlow (a !! i) (map (!! j) b) | j <- m] | i <- n]

matMult :: Num a => Matrix a -> Matrix a -> Matrix a
matMult a b = [[scalarProduct row col | col <- transpose b] | row <- a]

{- Exercise 2 -}

{- foldl (-) 0 [1..4] dissolves to  (((0 - 1) - 2) - 3) - 4
   while foldr (-) 0 [1..4] dissolves to 1 - (2 - (3 - (4 - 0)))

    so basically foldl starts from the left while foldr starts from the right.
    Note that the 'neutral element' 0 is used as the starting point for both functions,
    but foldl passes it as the first argument to the function while foldr passes it as the second argument.
    -}

-- Task 2.2
insertionSortRec :: Ord a => [a] -> [a]
insertionSortRec [] = []
insertionSortRec (x : xs) = insert (insertionSortRec xs)
  where
    insert [] = [x]
    insert (y : ys)
      | x < y = x : y : ys
      | otherwise = y : insert ys


-- insertionSortFold :: Ord a => [a] -> [a]
-- insertionSortFold (x :xs) = foldr (<= x) x xs
-- -- Task 2.3

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

exampleTree :: Tree Integer
exampleTree =
  let n x = Node Leaf x Leaf
   in Node (Node (n 1) 2 (n 3)) 4 (Node Leaf 5 (Node (n 6) 7 Leaf))

foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt _ b Leaf = b -- base case
foldt f b (Node l x r) = f (foldt f b l) x (foldt f b r) -- recursive case

addTree :: Num a => a -> a -> a -> a
addTree x y z = x + y + z

height :: Tree a -> Int
height = foldt (\l _ r -> 1 + l `max` r) 0

flatten :: Tree a -> [a]
flatten = foldt (\l x r -> l ++ [x] ++ r) []

mirror :: Tree a -> Tree a
mirror = foldt (\l x r -> Node r x l) Leaf

mapTree :: (a -> b) -> Tree a -> Tree b -- why do I have to use from a to b here? and a -> a is not working?
mapTree _ Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

showTree :: Show a => Tree a -> String
showTree tree@(Node l x r)= foldt (\l x r -> l ++ show x ++ "\n" ++ r) "" tree
--   where
--     indent = replicate (length indent + 2) ' '
--     middle =  x 
    -- check if l and r are Nodes or Leafs
    
-- showTree :: Show a => Tree a -> String
-- showTree Leaf = ""
-- showTree tree@(Node l x r) = foldt (\left middle right -> left ++ show middle ++ right) indent tree
--   where
--     initialIndent = "  "  -- Choose your initial indentation
--     indent = replicate (length initialIndent + 2) ' '

{- Exercise 3 -}

{- Tests -}

-- Tests for Exercise 1
tests1 = do
  putStrLn "Testing functional correctness of vector and matrix operations"
  testNeg (generateVec 5)
  testNeg testvecV
  testAdd (generateVec 5) (generateVec 5)
  testAdd testvecV testvecW
  testScalar (generateVec 5) (generateVec 5)
  testScalar testvecV testvecW
  testTranspose (generateMat 5)
  testTranspose testmatA
  testTranspose testmatB
  testMult (generateMat 4) (generateMat 4)
  testMult testmatA testmatB
  putStrLn $ replicate 80 '='
  putStrLn "Testing efficiency of vector and matrix operations"
  testTiming

-- Tests for Exercise 2
tests2 = sequence_ [testFoldt, testHeight, testFlatten, testMirror, testMapTree, testShowTree]

generateVec n = [1 .. n]

generateMat n = replicate n [1 .. n]

testvecV = [3, 1, -20, 15]

testvecW = [490, -2, 3, -5]

testmatA = [[1, 2, 3], [4, 5, 6]]

testmatB = [[7, 10, 11, 12], [8, 13, 14, 15], [9, 16, 17, 18]]

testEq a b s t
  | a == b = putStrLn $ t ++ " (OK)"
  | otherwise = putStrLn $ t ++ " (FAILED)\n" ++ "expected:\n" ++ s a ++ "\ncomputed:\n" ++ s b

showMat :: Show a => Matrix a -> String
showMat a = ("[" ++) $ drop 3 $ concatMap ((",\n " ++) . show) a ++ "]"

testNeg v = testEq (negateVecSlow v) (negateVec v) show ("testing: negateVec " ++ show v)

testAdd v w = testEq (vecAddSlow v w) (vecAdd v w) show ("testing: addVec " ++ show v ++ " " ++ show w)

testScalar v w = testEq (scalarProductSlow v w) (scalarProduct v w) show ("testing: scalarProduct " ++ show v ++ " " ++ show w)

testTranspose a = testEq (transposeSlow a) (transpose a) showMat ("testing: transpose " ++ show a)

testMult a b = testEq (matMultSlow a b) (matMult a b) showMat ("testing: matMult " ++ show a ++ " " ++ show b)

timedInt x
  | x == 0 = return True
  | otherwise = return False

timedMat a = timedInt (sum (map sum a))

timedVec v = timedInt (sum v)

timedCompute s i = do
  let to = 10 * 10 ^ 6 -- use 10 seconds timeout
  putStrLn s
  start <- getCPUTime
  res <- timeout to i
  case res of
    Nothing -> putStrLn "*** timeout after 10 seconds"
    Just _ -> do
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10 ^ 12) :: Double
      printf "Computation time: %0.3f sec\n" diff

testTiming = do
  let negSlow n = timedCompute ("n = " ++ show n) (timedVec (negateVecSlow (generateVec n)))
  let neg n = timedCompute ("n = " ++ show n) (timedVec (negateVec (generateVec n)))
  let addSlow n = timedCompute ("n = " ++ show n) (timedVec (vecAddSlow (generateVec n) (generateVec n)))
  let add n = timedCompute ("n = " ++ show n) (timedVec (vecAdd (generateVec n) (generateVec n)))
  let scalarSlow n = timedCompute ("n = " ++ show n) (timedInt (scalarProductSlow (generateVec n) (generateVec n)))
  let scalar n = timedCompute ("n = " ++ show n) (timedInt (scalarProduct (generateVec n) (generateVec n)))
  let transSlow n = timedCompute ("n = " ++ show n) (timedMat (transposeSlow (generateMat n)))
  let trans n = timedCompute ("n = " ++ show n) (timedMat (transpose (generateMat n)))
  let multSlow n = timedCompute ("n = " ++ show n) (timedMat (matMultSlow (generateMat n) (generateMat n)))
  let mult n = timedCompute ("n = " ++ show n) (timedMat (matMult (generateMat n) (generateMat n)))
  putStrLn "On our reference machine, each non-slow test needs at most 1 second."
  putStrLn "Your implementations should at least be faster than the slow ones"
  putStrLn "(for the same value of n)."
  putStrLn $ replicate 80 '='
  putStrLn "Negation Slow"
  mapM_ negSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Negation"
  mapM_ neg [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Addition Slow"
  mapM_ addSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Addition"
  mapM_ add [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Scalar Product Slow"
  mapM_ scalarSlow [1000, 10000, 100000]
  putStrLn $ replicate 80 '-'
  putStrLn "Scalar Product"
  mapM_ scalar [100000, 1000000, 10000000]
  putStrLn $ replicate 80 '='
  putStrLn "Transpose Slow"
  mapM_ transSlow [200, 400, 800]
  putStrLn $ replicate 80 '-'
  putStrLn "Transpose"
  mapM_ trans [800, 1600, 3200]
  putStrLn $ replicate 80 '='
  putStrLn "Matrix Multiplication Slow"
  mapM_ multSlow [50, 100, 150]
  putStrLn $ replicate 80 '-'
  putStrLn "Matrix Multiplication"
  mapM_ mult [150, 200, 300]

------------------
--- Ex 2 Tests ---
------------------

exampleTree2 = Leaf

exampleTree3 = Node Leaf 1 (Node Leaf (-3) Leaf)

testFoldt = do
  testFoldAux 28 (\l x r -> l + x + r) "(\\l x r -> l + x + r)" 0 (exampleTree :: Tree Integer)
  testFoldAux 1 (\_ _ _ -> 5) "(\\_ _ _ -> 5)" 1 (exampleTree2 :: Tree Double)
  testFoldAux 8 (\l x r -> l - x + r) "(\\l x r -> l - x + r)" 2 exampleTree3
  where
    testFoldAux expected f fstr e t = testEq expected (foldt f e t) show ("testing: foldt " ++ fstr ++ " " ++ show e ++ " $ " ++ show t)

testHeight = mapM_ (\(exp, t) -> testEq exp (height t) show ("testing: height " ++ show t)) [(4, exampleTree), (0, exampleTree2), (2, exampleTree3)]

testFlatten = mapM_ (\(exp, t) -> testEq exp (flatten t) show ("testing: flatten " ++ show t)) [([1, 2, 3, 4, 5, 6, 7], exampleTree), ([], exampleTree2), ([1, -3], exampleTree3)]

testMirror =
  mapM_
    (\(exp, t) -> testEq exp (show $ mirror t) show ("testing: mirror " ++ show t))
    [ ("Node (Node (Node Leaf 7 (Node Leaf 6 Leaf)) 5 Leaf) 4 (Node (Node Leaf 3 Leaf) 2 (Node Leaf 1 Leaf))", exampleTree),
      ("Leaf", exampleTree2),
      ("Node (Node Leaf (-3) Leaf) 1 Leaf", exampleTree3)
    ]

testMapTree = do
  testMapTreeAux "Node (Node (Node Leaf False Leaf) True (Node Leaf False Leaf)) True (Node Leaf False (Node (Node Leaf True Leaf) False Leaf))" even "even" (exampleTree :: Tree Integer)
  testMapTreeAux "Leaf" (const 10) "(const 10)" (exampleTree2 :: Tree String)
  testMapTreeAux "Node Leaf (-1) (Node Leaf 3 Leaf)" negate "negate" exampleTree3
  where
    testMapTreeAux expected (f :: a -> b) fstr t = testEq expected (show $ (mapTree f t :: Tree b)) show ("testing: mapTree " ++ fstr ++ " $ " ++ show t)

testShowTree =
  mapM_
    (\(exp, t) -> testEq exp (showTree t) ("testing: showTree $ " ++ show t))
    [ ("      1\n    /\n   2\n    \\\n      3\n /\n4\n \\\n   5\n    \\\n         6\n       /\n      7\n", exampleTree),
      ("", exampleTree2),
      ("1\n \\\n   -3\n", exampleTree3)
    ]
  where
    testEq expected res prefx
      | expected == res = do
          putStrLn $ prefx ++ " (OK)"
      | otherwise = do
          putStrLn $ replicate 80 '='
          putStrLn $ prefx ++ " (FAILED)"
          putStrLn $ "expected:\n" ++ expected
          putStrLn $ "computed:\n" ++ res
          putStrLn $ replicate 80 '='