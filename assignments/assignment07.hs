{- Exercise 1 -}

-- TODO: add *most general* types of div1 - div4
div1 = (/ 2)

div2 = (2 /)

div3 = (. (/ 2))

div4 = ((/ 2) .)

div5 f = f . div1

div6 x = \ f -> f (2 / x)

div7 (f, x) = div3 f x

div8 = \(f, x) -> f (x / 2)

-- Question 1.1
{-
What do div1 and div2 do? Give an example that shows the difference between div1 and div2.

-}

-- Question 1.2
{-
What do div3 and div4 do?  Give an example that shows the difference between div3 and div4.

-}

-- Question 1.3
{-
Which of the following pairs of functions are equal? Justify your answers.

(i) div3 and div5

(ii) div5 and flip div6

(iii) div7 and div8

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
mapEmployee n a s e = undefined

-- Question 2.2
nextYear :: [Employee] -> [Employee]
nextYear = undefined

-- Question 2.3

lowIncomeEmployees :: [Employee] -> [(Name, Salary)]
lowIncomeEmployees = undefined

-- Question 2.4
qsortBy :: (a -> a -> Bool) -> [a] -> [a]
qsortBy = undefined

-- Question 2.5
employeesByIncome :: [Employee] -> [Name]
employeesByIncome = undefined

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