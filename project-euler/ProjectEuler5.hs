-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- Must be even, must end in 0, sum of digits must be divisible by 3, sum of all digits must be divisible by 9, 2 last digits divisible by 4

smallestPositiveNumberDivisibleByAll :: [Integer] -> [Integer] -> [Integer]
smallestPositiveNumberDivisibleByAll listDivisibles range =
    take 1 $ filter (`divisibleByAll` listDivisibles) range
    where
        divisibleByAll :: Integer -> [Integer] -> Bool
        divisibleByAll n = all (\x -> n `rem` x == 0)


-- Answer: 232792560
-- Found: 232792560