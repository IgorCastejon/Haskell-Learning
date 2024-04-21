--By listing the first six prime numbers: $2, 3, 5, 7, 11$, and $13$, we can see that the $6$th prime is $13$.</p>
--What is the $10\,001$st prime number?</p>

-- >>> isPrime 3
-- True
-- >>> isPrime 7
-- True
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = isThereAnyNumberBetween1AndSqrtNThatDividesN
    where
        allNumbersBetween1AndSqrtN = [2..(sqrtInt n)]
        isThereAnyNumberBetween1AndSqrtNThatDividesN = all (\m -> (n `rem` m) /= 0) allNumbersBetween1AndSqrtN

-- >>> sqrtInt 2
-- 2
sqrtInt :: Integer -> Integer
sqrtInt = ceiling . sqrt . fromInteger


-- >>> solution
-- 104743
solution :: Integer
solution = allPrimes !! 10000

allPrimes :: [Integer]
allPrimes = filter isPrime [2..]

-- Answer: 104743
