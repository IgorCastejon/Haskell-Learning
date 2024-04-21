-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143?

-- A prime factor: is prime and divides the original number
-- Prime: only divisible by 1 and itself (rem = 0), so not divisible by any other number (rem /= 0). For optimization, we know that there can't be a number bigger than sqrt(n) that divides n.

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = isThereAnyNumberBetween1AndSqrtNThatDividesN
    where
        allNumbersBetween1AndSqrtN = [2..(sqrtInt n)]
        isThereAnyNumberBetween1AndSqrtNThatDividesN = all (\v -> (n `rem` v) /= 0) allNumbersBetween1AndSqrtN

isPrimeFactor :: Integer -> Integer -> Bool
isPrimeFactor n primeFactor
    = isPrime primeFactor && (n `rem` primeFactor) == 0

primeFactors :: Integer -> [Integer]
primeFactors x = filter (isPrimeFactor x) [2..(sqrtInt x)]

maxPrimeFactorsSolution :: Integer
maxPrimeFactorsSolution = maximum $ primeFactors 600851475143

sqrtInt :: Integer -> Integer
sqrtInt = ceiling . sqrt . fromInteger

-- Answer: 6857
-- Found: 6857

    