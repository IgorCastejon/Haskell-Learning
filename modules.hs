import qualified Data.List as List
import Data.Function (on)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Char
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

sort' :: (Eq a, Ord a) => [a] -> [a]
sort' = List.sort

--safeHead :: NonEmpty a -> a
--safeHead (x:|xs) = x

rle :: (Eq a) => [a] -> [(a, Int)]
rle = groupingsToTuplesWithNumberOfOccurences . List.group
    where
        groupingsToTuplesWithNumberOfOccurences =  map (\grouping@(x:_) -> (x, length grouping))

numCharsPerLine :: String -> [Int]
numCharsPerLine = map length . lines

--twoSum :: (Num a, Eq a) => a -> [a] -> Maybe (Int, Int)
--twoSum _ [] = Nothing
--twoSum target list =
--    let
--        differencesToTarget = map (target -) list
--        differencesToTargetWithIndexes = zip differencesToTarget [0..]
--        groupBySumToTarget = List.partition (\(a, _) (b, _) -> a + b == target) differencesToTargetWithIndexes
--        possiblePairThatSumsToTarget = List.find ((== 2) . length) groupBySumToTarget
--    in
        --fmap (\(_, idx1) (_, idx2) -> (idx1, idx2)) $ 
--        Just (0, 0)

addTwoNumbersLists :: NonEmpty.NonEmpty Integer -> NonEmpty.NonEmpty Integer -> Integer
addTwoNumbersLists x1 x2 = transformToInt x1 + transformToInt x2
    where
        transformToInt :: NonEmpty.NonEmpty Integer -> Integer
        transformToInt list = sum allNumbersInListMappedToBase10
            where
                listWithIndexes = NonEmpty.zip list (NonEmpty.fromList [0..])
                allNumbersInListMappedToBase10 = NonEmpty.map (\(x, idx) -> x * (10^idx)) listWithIndexes


addAnyNumbersLists :: NonEmpty.NonEmpty (NonEmpty.NonEmpty Integer) -> Integer
addAnyNumbersLists listOfLists = sum $ NonEmpty.map transformToInt listOfLists
    where
        transformToInt :: NonEmpty.NonEmpty Integer -> Integer
        transformToInt list = sum allNumbersInListMappedToBase10
            where
                listWithBase10Exponents = NonEmpty.zip list (NonEmpty.fromList [0..])
                allNumbersInListMappedToBase10 = NonEmpty.map (\(x, exp) -> x * (10^exp)) listWithBase10Exponents

sumTwo :: Integer -> [Integer] -> Maybe (Integer, Integer)
sumTwo target list = getIndexesOfPairThatSumsToTarget $ findPairThatSumsToTarget allPairsOfListWithIndexes
    where
        allPairsOfListWithIndexes = allPairs $ zip list [0..]
        findPairThatSumsToTarget = List.find (\ ((x, xIdx), (y, yIdx)) -> x + y == target)
        getIndexesOfPairThatSumsToTarget = fmap (\((x, xIdx), (y, yIdx)) -> (xIdx, yIdx))

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = pairElement x xs ++ allPairs xs

pairElement :: a -> [a] -> [(a, a)]
pairElement _ [] = []
pairElement elem (x:xs) = (elem, x) : pairElement elem xs

--allPermutations :: [a] -> [[a]]
--allPermutations [] = []
--allPermutations list@(x:xs) = map (x :) (allPermutations xs)

-- [1, 2, 3] -> 1: [2, 3], 1: [3, 2], 2: [1, 3], 2: [3, 1], 3: [1, 2], 3: [2, 1]

-- allPossibleListsStartingWithX -> 1 [2, 3, 4] -> allPossibleListsStartingWithX 2 [1, 2, 3, 4], [1, 2, 4, 3], [1, 3, 2, 4] -> [1, 4, 3, 2]

caesarShift :: String -> Int -> String
caesarShift string shiftedBy =
    let
        charsToInt = map Data.Char.ord string
        shiftedInts = map (+ shiftedBy) charsToInt
    in
        map Data.Char.chr shiftedInts

findKey :: Eq a => [(a, b)] -> a -> Maybe (a, b)
findKey list key = listToMaybe $ filter (\(k, v) -> k == key) list

findKeyVal :: Eq a => [(a, b)] -> a -> Maybe b
findKeyVal list key = fmap snd $ listToMaybe $ filter (\(k, v) -> k == key) list


