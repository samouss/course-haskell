import qualified Data.Map as Map

-- Value constructor
data Point = Point Float Float
data Shape = Circle Point Float | Rectangle Point Point

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs x2 - x1) * (abs y2 - y1)

data Person = Person
    { firstname :: String
    , lastname :: String
    , age :: Int
    } deriving (Show, Read, Eq)

-- Type constructor
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+1) (j+m) (k+n)

-- Enum
data Day = Monday | Tuesday | Wednesday | Friday | Saturday | Sunday
           deriving (Eq, Show, Read, Bounded, Enum)

-- Type synonyms (= alias)
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Locker example
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNum lockerMap = case Map.lookup lockerNum lockerMap of
    Nothing            -> Left $ "Locker number " ++ show lockerNum ++ " doesn't exist !"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNum ++ " is alreay taken !"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZAZ3S"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
