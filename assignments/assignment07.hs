{- Exercise 1 -}

-- TODO: add *most general* types of div1 - div4
div1 :: Fractional a => a -> a
div1 = (/ 2)

div2 :: Fractional a => a -> a
div2 = (2 /)

div3 :: Fractional a => (a -> b) -> a -> b
div3 = (. (/ 2))

div4 :: Fractional a => (a -> a) -> a -> a
div4 = ((/ 2) .)

div5 :: Fractional a => (a -> a) -> a -> a
div5 f = f . div1

-- div6 :: Fractional a => a -> (a -> a) -> a
div6 x = \ f -> f (2 / x)

div7 (f, x) = div3 f x

div8 = \(f, x) -> f (x / 2)

-- Question 1.1
{-
What do div1 and div2 do? Give an example that shows the difference between div1 and div2.
div1 and div2 are functions that divide a number by 2. div1 divides the number by 2, while div2 divides 2 by the number.
e.g.: div1 4 = 4 / 2 = 2
      div2 4 = 2 / 4 = 0.5
-}

-- Question 1.2
{-
What do div3 and div4 do?  Give an example that shows the difference between div3 and div4.
Both functions divide a number by 2 and apply another function to the result/ number. The difference is that div3 first divides
the number by 2 and then applies the function, while div4 first applies the function and then divides the result by 2.

div3 show 4 = show (4 / 2) = show 2 = "2"
div4 show 4 = (show 4) / 2 = "4" / 2 = ERROR
-}

-- Question 1.3
{-
Which of the following pairs of functions are equal? Justify your answers.

(i) div3 and div5
div3 and div5 are equal, because div5 is just a different notation for div3.
They have the same most general type, since they are both expecting a function followed by a number
as input and return the number divided by 2 and then the function applied to the result.

(ii) div5 and flip div6
div5 and flip div6 are not equal. Both functions apply a function to a number, but div6 divides 2 by the number before applying the function,
while div5 divides the number by 2 before applying the function.

(iii) div7 and div8
div7 and div8 are equal, since div8 is just a different notation for div3. It uses the lambda expression to apply the given function to the
result of the division of the number by 2. div3 does the same, but uses the composition operator instead of the lambda expression. div7 just
calls div3 with the given function and number as arguments.

-}

{- Exercise 2 -}

type Name = String

type Age = Integer

type Salary = Double

data Employee = Employee Name Age Salary deriving (Show)

employees :: [Employee]
employees =
  [ Employee "Alice" 28 50000.0,
    Employee "Bob" 35 60000.0,
    Employee "Charlie" 42 75000.0,
    Employee "David" 30 55000.0,
    Employee "Eva" 25 48000.0
  ]

getName :: Employee -> Name
getName (Employee n _ _) = n

getSalary :: Employee -> Salary
getSalary (Employee _ _ s) = s

-- Question 2.1
mapEmployee :: (Name -> Name) -> (Age -> Age) -> (Salary -> Salary) -> Employee -> Employee
mapEmployee updateName updateAge updateSalary (Employee n a s) = Employee (updateName n) (updateAge a) (updateSalary s) 

updateName :: (Name -> Name) -> Employee -> Employee
updateName f = mapEmployee f id id

updateAge :: (Age -> Age) -> Employee -> Employee
updateAge f = mapEmployee id f id

updateSalary :: (Salary -> Salary) -> Employee -> Employee
updateSalary f = mapEmployee id id f

-- Question 2.2
nextYear :: [Employee] -> [Employee]
nextYear = map (updateAge (+ 1)) . map (updateSalary (* 1.2)) 

-- Question 2.3

lowIncomeEmployees :: [Employee] -> [(Name, Salary)]
lowIncomeEmployees = map (\e -> (getName e, getSalary e)) . filter (\e -> getSalary e < 60000) 

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = -- x is pivot element
  qsort (filter (>= x) xs) ++ [x] ++ qsort (filter (< x) xs)

-- Question 2.4
qsortBy :: (a -> a -> Bool) -> [a] -> [a]
qsortBy f [] = []
qsortBy f (x : xs) = qsortBy f (filter (not . (f x)) xs) ++ [x] ++ qsortBy f (filter (f x) xs)
-- why does this work? I do not understand why i have to negate the first filter and not the second one

-- Question 2.5
employeesByIncome :: [Employee] -> [Name]
employeesByIncome = map getName . qsortBy (\e1 e2 -> getSalary e1 >= getSalary e2)
-- TESTS --

checkEx2 = mapM_ putStrLn [test1, test2, test3, test4, test5]

test1 = check "nextYear" (show [Employee "Alice" 29 60000.0, Employee "Bob" 36 72000.0, Employee "Charlie" 43 90000.0, Employee "David" 31 66000.0, Employee "Eva" 26 57600.0]) (nextYear employees)

test2 = check "lowIncomeEmployees" (show [("Alice", 50000.0), ("David", 55000.0), ("Eva", 48000.0)]) (lowIncomeEmployees employees)

test3 = check "qsortBy1" (show [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]) (qsortBy (>=) [1 .. 10])

test4 = check "qsortBy2" (show ["hi", "and", "world", "Innsbruck"]) (qsortBy (\xs ys -> length xs <= length ys) ["hi", "world", "and", "Innsbruck"])

test5 = check "employeesByIncome" (show ["Charlie", "Bob", "David", "Alice", "Eva"]) (employeesByIncome employees)

check :: Show a => [Char] -> String -> a -> [Char]
check name e c =
  "*** "
    ++ name
    ++ ": "
    ++ ( if show c == e
           then "OK"
           else "ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'"
       )