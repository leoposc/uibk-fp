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

today :: Date
today = DMY 16 October 2023