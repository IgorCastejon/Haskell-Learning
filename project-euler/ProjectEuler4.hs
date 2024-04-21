import Data.Foldable (maximumBy)
import Data.Ord qualified as Ord
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99
-- Find the largest palindrome made from the product of two 3-digit numbers

isPalindromicNumber :: Integer -> Bool
isPalindromicNumber n =
    let nAsString = show n
    in isStringPalindrome nAsString

isStringPalindrome :: String -> Bool
isStringPalindrome string = string == reverse string

--solutionLargestPalindromeFromProductOfTwo3DigitsNumbers :: (Integer, Integer, Integer)
--solutionLargestPalindromeFromProductOfTwo3DigitsNumbers =
--    largestPalindromeFromProductOfTwo3DigitsNumbers
--    where
--        allPossibleMultiplicationsOfTwo3DigitsNumbers = zipWith (\a b -> (a, b, a*b)) [100..999] [100..999]
--        allPalindromicNumbersResultingFromMultiplicationsOfTwo3DigitsNumbers = filter (\(a, b, product) -> isPalindromicNumber product) allPossibleMultiplicationsOfTwo3DigitsNumbers
--        largestPalindromeFromProductOfTwo3DigitsNumbers = maximumBy (Ord.comparing (\(a, b, product) -> product)) allPalindromicNumbersResultingFromMultiplicationsOfTwo3DigitsNumbers
naiveMultiplicationTable :: [(Integer, Integer, Integer)]
naiveMultiplicationTable = 
    loop 1
    where 
        inner :: Integer -> [(Integer, Integer, Integer)]
        inner n = map (\a -> (n, a, n*a)) [1..9]
        loop n
            | n >= 9 = inner n
            | otherwise = inner n ++ loop (n+1)

multiplicationTable :: [(Integer, Integer, Integer)]
multiplicationTable = [ (x, y, x * y) | x <- [1..9], y <-[1..9]]


solutionLargestPalindromeFromProductOfTwo3DigitsNumbers :: (Integer, Integer, Integer)
solutionLargestPalindromeFromProductOfTwo3DigitsNumbers =
    largestPalindromeFromProductOfTwo3DigitsNumbers
    where
        range = [100..999]
        allPossibleMultiplicationsOfTwo3DigitsNumbers = [(x, y, x*y) | x <- range, y <- range]
        allPalindromicNumbersResultingFromMultiplicationsOfTwo3DigitsNumbers = filter (\(a, b, product) -> isPalindromicNumber product) allPossibleMultiplicationsOfTwo3DigitsNumbers
        largestPalindromeFromProductOfTwo3DigitsNumbers = maximumBy (Ord.comparing (\(a, b, product) -> product)) allPalindromicNumbersResultingFromMultiplicationsOfTwo3DigitsNumbers


largestPalindromeFromProductOfTwoNDigitsNumbers :: Integer -> (Integer, Integer, Integer)
largestPalindromeFromProductOfTwoNDigitsNumbers n =
    largestPalindromeFromProductOfTwoNDigitsNumbers
    where
        range = [10^(n-1)..10^n - 1]
        allPossiblePalindromicMultiplicationsOfTwoNDigitsNumbers = [(x, y, x*y) | x <- range, y <- range, isPalindromicNumber (x*y)]
        largestPalindromeFromProductOfTwoNDigitsNumbers = maximumBy (Ord.comparing (\(a, b, product) -> product)) allPossiblePalindromicMultiplicationsOfTwoNDigitsNumbers


-- Answer: 906609
-- Found: 993 * 913 = 906609