{-
You are given an integer array height of length n. There are n vertical lines drawn such that the two endpoints of the ith line are (i, 0) and (i, height[i]).

Find two lines that together with the x-axis form a container, such that the container contains the most water.

Return the maximum amount of water a container can store.

Notice that you may not slant the container.

https://leetcode.com/problems/container-with-most-water/

Ex 1:
Input: height = [1,8,6,2,5,4,8,3,7]
Output: 49

Ex 2:
Input: height = [1,1]
Output: 1

Constraints:

n == height.length
2 <= n <= 105
0 <= height[i] <= 104

-}

import Data.Maybe qualified as Maybe

newtype Height
    = Height Int
    deriving (Show, Eq, Ord)

getHeight :: Height -> Int
getHeight (Height x) = x

mkHeight :: Int -> Maybe Height
mkHeight height
    | height >= 0 && height <= 104 = Just $ Height height
    | otherwise = Nothing

-- >>> (mkHeight (-1)) == Nothing
-- True

-- >>> (mkHeight (0)) == (Just $ Height 0)
-- True

-- >>> (mkHeight (105)) == (Nothing)
-- True

newtype Heights
    = Heights [Height]
    deriving (Show, Eq)

mkHeights :: [Height] -> Maybe Heights
mkHeights list
    | sizeList < 2 || sizeList > 105 = Nothing
    | otherwise = Just $ Heights list
    where sizeList = length list


-- >>> mkHeights [1]
-- Nothing

-- >>> mkHeights [1, 2]
-- Just (Heights [1,2])


newtype Area
    = Area Int
    deriving (Show, Eq, Ord)

mostWater :: Heights -> Area
mostWater (Heights heights) = 
    calculateMaximumContainerArea heights

calculateMaximumContainerArea :: [Height] -> Area
calculateMaximumContainerArea [] = Area 0
calculateMaximumContainerArea [height] = Area 0
calculateMaximumContainerArea [Height x, Height y] = Area (min x y)
calculateMaximumContainerArea heights =
    max maxContainerConsideringFirstElementAsStartingPoint maxContainerConsideringNextElementsAsStartingPoint
    where
        maxContainerConsideringFirstElementAsStartingPoint = calculateMaximumContainerAreaStartingFromFirst heights
        maxContainerConsideringNextElementsAsStartingPoint = (calculateMaximumContainerArea . drop 1) heights

calculateMaximumContainerAreaStartingFromFirst :: [Height] -> Area
calculateMaximumContainerAreaStartingFromFirst heights =
    let
        heightsWithIndexes :: [(Height, Int)] = zip heights [0..]
        heightOfFirstElem :: Int = getHeight $ head heights
        allPossibleAreasStartingFromFirst :: [Area] = 
            [
                let 
                    base = snd x
                    minHeightThatHoldsWater = min heightOfFirstElem ((getHeight . fst) x)
                in 
                    Area ( base * minHeightThatHoldsWater) 
                | x <- heightsWithIndexes
            ]
    in 
        maximum allPossibleAreasStartingFromFirst

-- >>> calculateMaximumContainerAreaStartingFromFirst [Height 1, Height 1]
-- Area 1

-- >>> calculateMaximumContainerAreaStartingFromFirst [Height 1, Height 1, Height 3]
-- Area 2

-- >>> calculateMaximumContainerAreaStartingFromFirst [Height 0, Height 1, Height 3]
-- Area 0

-- >>> calculateMaximumContainerAreaStartingFromFirst [Height 4, Height 1, Height 3]
-- Area 6

callMostWater :: [Int] -> Maybe Area
callMostWater list = mostWater <$> (mkHeights =<< mapM mkHeight list)

-- >>> callMostWater [0, 0] == Just (Area 0)
-- True

-- >>> callMostWater [0, 1] == Just (Area 0)
-- True

-- >>> callMostWater [1, 1] == Just (Area 1)
-- True

-- >>> callMostWater [1, 2] == Just (Area 1)
-- True

-- >>> callMostWater [2, 1] == Just (Area 1)
-- True

-- >>> callMostWater [2, 2] == Just (Area 2)
-- True

-- >>> callMostWater [3, 3] == Just (Area 3)
-- True

-- >>> callMostWater [2, 3] == Just (Area 2)
-- True

-- >>> callMostWater [0, 0, 0] == Just (Area 0)
-- True

-- >>> callMostWater [1, 2, 0] == Just (Area 1)
-- True

-- >>> callMostWater [0, 11, 11] == Just (Area 11)
-- True

-- >>> callMostWater [0, 0, 1] == Just (Area 0)
-- True

-- >>> callMostWater [1, 0, 1] == Just (Area 2)
-- True


-- >>> callMostWater [4, 0, 1] == Just (Area 2)
-- True

-- >>> callMostWater [4, 0, 4] == Just (Area 8)
-- True

-- >>> callMostWater [0, 0, 0, 0] == Just (Area 0)
-- True

-- >>> callMostWater [0, 0, 0, 1] == Just (Area 0)
-- True

-- >>> callMostWater [0, 0, 1, 1] == Just (Area 1)
-- True

-- >>> callMostWater [4, 0, 3, 1] == Just (Area 6)
-- True

-- >>> callMostWater [4, 0, 3, 5] == Just (Area 12)
-- True

-- >>> callMostWater [5, 0, 3, 5] == Just (Area 15)
-- True

-- >>> callMostWater [5, 0, 3, 5, 6] == Just (Area 20)
-- True

-- >>> callMostWater [0, 5, 3, 5, 6] == Just (Area 15)
-- True

-- >>> callMostWater [0, 3, 5, 5, 6] == Just (Area 10)
-- True

-- >>> callMostWater [0, 3, 5, 11, 11] == Just (Area 11)
-- True

-- >>> callMostWater [1,8,6,2,5,4,8,3,7]
-- Just (Area 49)

-- >>> callMostWater [1,8,6,2,5,4,8,3,8]
-- Just (Area 56)

-- >>> callMostWater [1,3,6,2,5,4,8,3,8]
-- Just (Area 36)

-- >>> mapM mkHeight [1, 1]
-- Just [Height 1,Height 1]

listSizeN :: Int -> [Int]
listSizeN n = concat (replicate n [1])

apply :: Maybe Area
apply = do
    let x :: [Maybe Height] = map mkHeight [1, 1]
    y :: [Height] <- sequence x
    z :: Heights <- mkHeights y
    pure $ mostWater z