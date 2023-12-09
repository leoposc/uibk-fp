module TreeModule (Tree(..), splitAtLevel, fillXs) where

data Tree a = Node a (Tree a) (Tree a) | X deriving Show

exampleTree = Node 1 (Node 2 X X) (Node 3 X (Node 4 X X))

takeLevels :: Int -> Tree a -> Tree a
takeLevels _ X = X
takeLevels i (Node x l r)
    | i <= 0 = X
    | otherwise = Node x (takeLevels (i-1) l) (takeLevels (i-1) r)

dropLevels :: Int -> Tree a -> [Tree a]
dropLevels _ X = [X]
dropLevels i t@(Node _ l r)
    | i <= 0 = [t]
    | otherwise = dropLevels (i-1) l ++ dropLevels (i-1) r

splitAtLevel :: Int -> Tree a -> (Tree a, [Tree a])
splitAtLevel _ X = (X, [X])
splitAtLevel i t@(Node x l r)
  | i <= 0 = (X, [t])
  | otherwise = (Node x t1 t2, ts1 ++ ts2)
  where
    (t1, ts1) = splitAtLevel (i-1) l
    (t2, ts2) = splitAtLevel (i-1) r

fillXs :: Tree a -> [Tree a] -> (Tree a, [Tree a])
fillXs t [] = (t, [])
fillXs X (t:ts) = (t, ts)
fillXs (Node x l r) ts = (Node x t1 t2, ts2)
  where
  (t1, ts1) = fillXs l ts
  (t2, ts2) = fillXs r ts1
