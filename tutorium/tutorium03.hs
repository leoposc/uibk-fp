
-- the List type from the VO slide 19.
data List = Empty | Cons Integer List deriving Show


-- define these functions, but use pattern matching


-- this function should return true, if the list is empty and false otherwise
isEmpty :: List -> Bool
isEmpty Empty = True
isEmpty _ = False

-- this function should test if the list starts with a specific number, what is the reasonable result for an empty list? 
startsWith :: List -> Integer -> Bool
startsWith Empty _ = False
startsWith (Cons x _) y = x == y

-- this function should replace the first element of the list with the argument and same as before, what is a reasonable way of handling an empty list?  
replaceFirst :: List -> Integer -> List
replaceFirst empty@Empty _ = empty
replaceFirst (Cons _ list2) y = Cons y list2
