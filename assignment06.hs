{- Exercise 1 -}

data Rat = Rat Integer Integer 

-- Question 1
normaliseRat :: Rat -> Rat
normaliseRat (Rat 0 d) = Rat 0 1
normaliseRat (Rat n d) =
    let commonDivisor = gcd (abs n) (abs d)
        sign = if (n * d) < 0 then -1 else 1
    in Rat (sign * abs (n `div` commonDivisor)) (abs (d `div` commonDivisor))


createRat :: Integer -> Integer -> Rat
createRat a b = normaliseRat (Rat a b)

-- Question 2
instance Eq Rat where
    Rat n1 d1 == Rat n2 d2 = 
        let normedRat1@(Rat num1 den1) = normaliseRat (Rat n1 d1)
            normedRat2@(Rat num2 den2) = normaliseRat (Rat n2 d2)
        in num1 == num2 && den1 == den2
    -- Rat n1 d1 == Rat n2 d2 = n1 == n2 && d2 == d2 

instance Ord Rat

-- Question 3
instance Show Rat where
  show r = undefined

-- Question 4
instance Num Rat

{- Exercise 2 -}

class MonoidC a where
  binop :: a -> a -> a
  neutral :: a

-- Question 1

-- define MonoidC instances for Double and Integer (addition) and Lists (append)

-- Question 2
data Tally = PM String

normalise :: String -> String
normalise = undefined

-- Question 3
-- make Tally instance of Eq, Show and MonoidC

-- Question 4
combine :: undefined
combine = undefined

  
{- executable Tests are only available for Exercise 1; 
   for Exercise 2, you first have to add the class instantions and can then manually test the following: 
   combine [1 .. 100 :: Integer] = 5050
   combine (map (\x -> [x]) ['a' .. 'z']) = "abcdefghijklmnopqrstuvwxyz"
   combine [PM "++--+", PM "+---+", PM "---+"] = PM "--"
   combine ["++--+", "+---+", "---+"] = "++--++---+---+"
   normalise "++--+++-++++" = "++++++" 
   PM "++--+++-++++" == PM "++++++" 
   PM "+--+" == PM ""
   PM "+--+" /= PM "+-+"
-}
testRats = do
  check
    "normaliseRat"
    "[True,True,True,False]"
    ( map
        ( \(r1, r2) -> case (normaliseRat r1, normaliseRat r2) of
            (Rat n1 d1, Rat n2 d2) -> (n1, d1) == (n2, d2)
        )
        [(Rat (-1) (-2), Rat 2 4), (Rat (-3) 7, Rat 6 (-14)), (Rat 0 3, Rat 0 4), (Rat 1 3, Rat (-1) 3)]
    )
  check "createRat" "-1/2" (createRat 5 (-10))
  check "Equality on Rats" "[True,False,False]" [Rat 3 5 == Rat (-9) (-15), Rat 3 5 == Rat 5 3, Rat 3 5 == Rat 3 (-5)]
  check
    "Order on Rats"
    "[True,False,True]"
    [Rat 3 5 < Rat 3 4, Rat 2 4 < Rat 2 4, Rat (-5) 3 < Rat 10 1]
  check "Show on Rats" "[3,4/5,-1/2]" [Rat 3 1, Rat 4 5, Rat 1 (-2)]
  check
    "Num on Rats"
    "[1/2,7/12,3/4,-1]"
    [3 * Rat 1 6, Rat 1 3 + Rat 1 4, abs (Rat (-3) 4), signum (negate (Rat 3 4))]

check name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e
    then putStrLn "OK"
    else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")