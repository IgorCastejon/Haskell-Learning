-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 2
fib n = fib (n-1) + fib (n-2)

allFibonacciValuesBelowOrEqual4kk :: [Integer]
allFibonacciValuesBelowOrEqual4kk = takeWhile(<= 4000000) $ map fib [0..]

allEvenFibonacciValuesBelowOrEqual4kk :: [Integer]
allEvenFibonacciValuesBelowOrEqual4kk = filter even allFibonacciValuesBelowOrEqual4kk

sumAllEvenFibonacciValuesBelowOrEqual4kk :: Integer
sumAllEvenFibonacciValuesBelowOrEqual4kk = sum allEvenFibonacciValuesBelowOrEqual4kk

-- Answer = 4613732
-- Found = 4613732