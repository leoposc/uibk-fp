data Expr = Variable String | Number Integer | Add Expr Expr | Negate Expr deriving Show
data Assign = Empty | Assign String Integer Assign deriving Show

exampleExpr = Negate (Add (Variable "x") (Negate (Add (Variable "y") (Number 3))))
exampleAssign = Assign "x" 5 (Assign "y" 12 Empty)

ite True x y = x
ite False x y = y

-- search for String in Assign and return the value
value :: String -> Assign -> Integer
value s Empty = error ("variable " ++ s ++ " not found")
value s (Assign s2 i assign2) = ite (s == s2) i (value s assign2)

eval :: Assign -> Expr -> Integer
eval _ (Number i) = i
eval assign (Variable s) = value s assign
eval assign (Negate expr) = -1 * eval assign expr
eval assign (Add e1 e2) = eval assign e1 + eval assign e2

containsVar :: String -> Assign -> Bool 
containsVar _ Empty = False
containsVar s (Assign s2 _ assign) = s == s2 || containsVar s assign 

substitute :: Assign -> Expr -> Expr
substitute _ (Number i) = Number i
substitute assign (Variable s) = ite (containsVar s assign) (Number (value s assign)) (Variable s)
substitute assign (Negate expr) = Negate (substitute assign expr)
substitute assign (Add e1 e2) = Add (substitute assign e1) (substitute assign e2)

normalize :: Expr -> Expr
normalize (Number i) = Number i
normalize (Variable s) = Variable s
normalize (Negate (Number i)) = Number (i * (-1))
normalize (Negate (Negate expr)) = normalize expr
normalize (Negate (Variable s)) = Negate (Variable s)
normalize (Add e1 e2) = Add (normalize e1) (normalize e2)
normalize (Negate (Add e1 e2)) = Add (normalize(Negate e1)) (normalize(Negate e2))

-----------
-- Tests --
-----------

testsAll = tests1 >> tests2 >> tests3 >> tests4 >> tests5

tests1 = checkAll "exercise 1" [
  (("x",exampleAssign),5),
  (("y",exampleAssign),12),
  (("y",exA),3),
  (("z",exB),9),
  (("x",exA),4),
  (("var",exA),7)
    ] (show . snd) (\ ((x,a),_) -> "value " ++ show x ++ " (" ++ show a ++ ")") 
      (uncurry value . fst)
      
tests2 = checkAll "exercise 2" [
  ((Empty, Number 5), 5),
  ((exampleAssign,exampleExpr),10),
  ((exA,exE),6),
  ((exA,exampleExpr),2)
    ] (show . snd) (\ ((a,e),_) -> "eval (" ++ show a ++ ") (" ++ show e ++ ")") 
      (uncurry eval . fst)
      
tests3 = checkAll "exercise 3" [
  (("x",exampleAssign),True),
  (("y",exampleAssign),True),
  (("z",exampleAssign),False),
  (("y",exA),True),
  (("variable",exB),False),
  (("var",exA),True)
    ] (show . snd) (\ ((x,a),_) -> "containsVar " ++ show x ++ " (" ++ show a ++ ")") 
      (uncurry containsVar . fst)
      
tests4 = checkAll "exercise 4" [
  ((Empty, Number 5), (Number 5)),
  ((exampleAssign,exampleExpr),Negate (Add (Number 5) (Negate (Add (Number 12) (Number 3))))),
  ((Empty,exampleExpr),exampleExpr),
  ((exampleAssign,exE),Negate (Add (Number (-3)) (Add (Number 5) (Negate (Variable "var"))))),
  ((Empty,exE),exE),
  ((exA,exampleExpr),Negate (Add (Number 4) (Negate (Add (Number 3) (Number 3)))))
    ] (show . snd) (\ ((a,e),_) -> "substitute (" ++ show a ++ ") (" ++ show e ++ ")") 
      (uncurry substitute . fst)
      
tests5 = checkAll "exercise 5" [
  (exampleExpr, Add (Negate (Variable "x")) (Add (Variable "y") (Number 3))),
  (exE, Add (Number 3) (Add (Negate (Variable "x")) (Variable "var"))),
  (Negate (Add (Number 5) (Number 8)), Add (Number (-5)) (Number (-8)))
    ] (show . snd) (\ (e,_) -> "normalize (" ++ show e ++ ")") 
      (normalize . fst)
      

-- internal construction of tests
aol = foldr (\ (x,y) -> Assign x y) Empty

exA = aol [("y",3),("x",4),("var",7)]
exB = aol [("z",9)]

exE = Negate (Add (Number (-3)) (Add (Variable "x") (Negate (Variable "var"))))

checkAll
  :: Show a =>
     String -> [b] -> (b -> String) -> (b -> String) -> (b -> a) -> IO ()
checkAll name xs e err c = do
    putStr ("*** " ++ name ++ ": ")
    let errors = filter (\x -> show (c x) /= e x) xs
    if null errors then putStrLn "OK"
    else do
        let x = head errors
        putStrLn ("ERROR; expexted '" ++ e x ++ "', but found '" ++ show (c x) 
          ++ "' for '" ++ err x ++ "'")
