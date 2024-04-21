{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import qualified Data.Map as M
{-# HLINT ignore "Use foldr" #-}
doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y


doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x*2


doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conan'Obrien = "Conan"

reverse' list = if null list
                    then []
                    else last list : reverse' (init list)

boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

doubleEveryOther :: Num a => [a] -> [a]
doubleEveryOther list = if null list || length list == 1
                            then list
                            else doubleLastElementOfList list : getSecondLastElementOfList list : (doubleEveryOther . removeTwoLastElementsFromList) list


doubleLastElementOfList :: Num a => [a] -> a
doubleLastElementOfList = doubleMe . last

getSecondLastElementOfList :: Num a => [a] -> a
getSecondLastElementOfList = last . init

removeTwoLastElementsFromList :: [a] -> [a]
removeTwoLastElementsFromList = init . init


rightTriangles :: [(Integer, Integer, Integer)]
rightTriangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2 ]

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky!"
lucky x = "Out of luck!"

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

length'' :: [a] -> Integer
length'' x = sum [1 | _ <- x]

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + length' xs

sinks :: (RealFloat a) => a -> a -> Bool
sinks mass volume
    | mass / volume <= 1000 = False
    | otherwise = True

max' :: (Ord a) => a -> a -> a
max' x y
    | x <= y = y
    | otherwise = x

sinks' :: (RealFloat a) => a -> a -> Bool
sinks' mass volume
    | mass / volume <= waterDensity = False
    | otherwise = True
    where
        density = mass / volume
        waterDensity = 1000.0

length''' :: [a] -> Integer
length''' [] = 0
length''' (_:xs) = 1 + lengthRestOfList
    where
        lengthRestOfList = length' xs


doubleEveryOtherFromBack :: Num a => [a] -> [a]
doubleEveryOtherFromBack list = reverse $ doubleEveryOtherFromFront $ reverse list
    where
        doubleEveryOtherFromFront [] = []
        doubleEveryOtherFromFront [x] = [x]
        doubleEveryOtherFromFront (x:y:xs) = x : 2*y : doubleEveryOtherFromFront xs


maximumOfList :: (Ord a) => [a] -> a
maximumOfList [] = error "Can't have maximum of empty list"
maximumOfList [x] = x
maximumOfList (head:tail) = max head maximumOfTail
    where
        maximumOfTail = maximumOfList tail

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' n _
    | n <= 0 = []
take' n (x:xs) = x : take' (n-1) xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:tail) = lessOrEqualThanPivot ++ [pivot] ++ greaterThanPivot
    where
        lessOrEqualThanPivot = filter (<= pivot) tail
        greaterThanPivot = filter (> pivot) tail

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (pivot:tail) = lessOrEqualThanPivot ++ [pivot] ++ greaterThanPivot
    where
        lessOrEqualThanPivot = [x | x <- tail, x <= pivot]
        greaterThanPivot = [x | x <- tail, x > pivot]


mul :: Integer -> Integer -> Integer
mul x y = x * y

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

largestNumberUnder100000DivisibleBy3829 :: Integer =
    let
        allNumbersUnder100000 :: [Integer] = [1..100000]
        divisibleBy3829 x = rem x 3829 == 0
    in
        maximum $ filter divisibleBy3829 allNumbersUnder100000

largestNumberUnderXDivisibleByY :: Integer -> Integer -> Integer
largestNumberUnderXDivisibleByY x y =
    let
        numbersUnderX = [1..x]
        divisibleByY x = rem x y == 0
    in
        maximum $ filter divisibleByY numbersUnderX

largestNumberUnderXDivisibleByY' :: Integer -> Integer -> Integer
largestNumberUnderXDivisibleByY' x y =
    maximum $ filter divisibleByY numbersUnderX
    where
        numbersUnderX = [1..x]
        divisibleByY x = rem x y == 0

sumOfOddSquaresUnderX :: Integer -> (Integer, [Integer])
sumOfOddSquaresUnderX x =
    (sum oddSquaredNumbersUnderX, oddSquaredNumbersUnderX)
    where
        squaredNumbersUnderX = map (^2) [1..x]
        oddSquaredNumbersUnderX = filter odd squaredNumbersUnderX

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain x
    | even x = x : collatzChain (x `div` 2)
    | odd x = x : collatzChain ((x * 3) + 1)

numChainsGreaterThan15Between1And100 :: Int
numChainsGreaterThan15Between1And100 =
    length $ filter isGreaterThan15 $ map collatzChain [1..100]
    where
        isGreaterThan15 x = length x > 15

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' element list = foldr (\x acc -> if x == element then True else acc) False list

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Num a, Ord a) => [a] -> a
maximum'' list = foldr1 (\x acc -> if x >= acc then x else acc) list


maximum''' :: (Num a, Ord a) => [a] -> a
maximum''' [] = error "aaaa"
maximum''' [x] = x
maximum''' (x:xs)
    | x > maxRestOfList = x
    | otherwise = maxRestOfList
    where
        maxRestOfList = maximum''' xs

reverse'' :: [a] -> [a]
reverse'' list = foldr (\x acc -> acc ++ [x]) [] list
-- [1, 2, 3] -> [] ++ [3] -> [] ++ [3] ++ [2] -> [] ++ [3] ++ [2] ++ [1]

reverse''' :: [a] -> [a]
reverse''' list = foldl (\acc x -> x : acc) [] list
-- [1, 2, 3] -> 1 : [] -> 2 : 1 : [] -> 3 : 2 : 1 : []

-- foldr acc ++ [x] == foldl x : acc - to reverse
-- foldl acc ++ [x] == foldr x : acc - normal order

product'' :: (Num a) => [a] -> a
product'' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p list = foldr (\x acc -> if p x then x : acc else acc) [] list

head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)
-- [1, 2, 3] -> [3] -> [2] -> [1]

head''' :: [a] -> a
head''' = foldl1 (\acc _ -> acc)
-- [1, 2, 3] -> [1] -> [1] -> [1

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)
-- [1, 2, 3] -> [1] -> [1] -> [1]

last''' :: [a] -> a
last''' = foldr1 (\_ acc -> acc)
-- [1, 2, 3] -> [3] -> [3] -> [3]

factorial' :: Integer -> Integer
factorial' x = foldr (*) 1 [1..x]

--allMultiplesOfXUnderY :: [Integer] -> Integer -> [Integer]
--allMultiplesOfXUnderY divisibles y = foldr (\x acc -> any (\k -> k rem) ) [1..y]

allMultiplesOf3Or5UnderY :: (Integral a) => a -> [a]
allMultiplesOf3Or5UnderY y = filter (\x -> (rem x 3 == 0) || (rem x 5 == 0)) [1..(y-1)]

listOfRemainders :: Integral c => [c] -> [c -> c]
listOfRemainders list = map (flip rem) list

xDivisibleByAnyInList :: Integral b => b -> [b] -> Bool
xDivisibleByAnyInList x list = any ((== 0) . ($ x)) (listOfRemainders list)

allMultiplesOfAnyInListUnderY :: [Integer] -> Integer -> [Integer]
allMultiplesOfAnyInListUnderY list y =
    let
        allNumbersUnderY = [1..(y-1)]
        numbersUnderYDivisibleByAnyInList = filter (`xDivisibleByAnyInList` list) allNumbersUnderY
    in
        numbersUnderYDivisibleByAnyInList


data Point =
    Point Float Float
    deriving (Show)

data Shape =
    Circle Point Float
    | Rectangle Point Point
    deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 x2) (Point y1 y2)) = abs (x2 - x1) * abs (y2 - y1)

--data Person = 
--    Person { 
--        firstName :: String, 
--        lastName :: String, 
--        age :: Int, 
--        height :: Float, 
--        phoneNumber :: String, 
--        flavor :: String
--    } deriving (Show)

data OtherPerson =
    OtherPerson {
        firstName :: String,
        lastName :: String
    } deriving (Eq)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday  
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

data Tree a 
    = Node a (Tree a) (Tree a)
    | EmptyTree
    deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyTree = singleton x
insertTree x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insertTree x left) right
    | x > a = Node a left (insertTree x right)

elemIsInTree :: (Ord a) => a -> Tree a -> Bool
elemIsInTree elem EmptyTree = False
elemIsInTree elem (Node a left right)
    | elem == a = True
    | elem > a = elemIsInTree elem right 
    | elem < a = elemIsInTree elem left 

data List a =
    EmptyList 
    | Cons a (List a)
    deriving (Show, Eq)

infixr 5 -:-
(-:-) :: a -> List a -> List a
(-:-) x list = Cons x list


data TrafficLight 
    = GreenLight
    | YellowLight
    | RedLight

instance Eq TrafficLight where
    (==) :: TrafficLight -> TrafficLight -> Bool
    GreenLight == GreenLight = True
    YellowLight == YellowLight = True
    RedLight == RedLight = True
    _ == _ = False

instance Show TrafficLight where
    show :: TrafficLight -> String
    show RedLight = "Red"
    show YellowLight = "Yellow"
    show GreenLight = "Green"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno :: Int -> Bool
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno :: [a] -> Bool
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno :: Bool -> Bool
    yesno = id

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

