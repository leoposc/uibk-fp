data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show)

data Date -- name of type
    = DMY -- name of constructor
        Int -- day
        Month -- month
        Integer -- year
    deriving (Show)
    
data FridgeObject
    = Food { 
        name :: String,
        expiryDate :: Date,
        quantity :: Int 
    }
    | Liquid {
        name :: String,
        expiryDate :: Date, 
        volume :: Double }
  deriving (Show)


applesA :: FridgeObject
applesA = Food "apples" (DMY 31 October 2023) 4

milkB :: FridgeObject
milkB = Liquid "milk" (DMY 4 August 2023) 2.3

lemonsC :: FridgeObject
lemonsC = Food "lemon" (DMY 3 November 2023) 6

newtype FridgeList
    = FridgeList [FridgeObject]
  deriving (Show)

exampleFridgeList :: FridgeList
exampleFridgeList = FridgeList [applesA, applesA, milkB]

{-  Since the order does matter in a list,
    exampleFridgeList is a unique representation. 
-}
