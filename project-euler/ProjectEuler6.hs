{- 
    The sum of the squares of the first ten natural numbers is:
        1^2 + 2^2 + 3^2 ... + 10^2 = 385.
    The square of the sum of the first ten natural numbers is:
        (1 + 2 + 3 + ... + 10)^2 = 55^2 = 3025
    Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is:
        3025 - 385 = 2640

    Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

square :: Num a => a -> a
square x = x^2

sumOfSquares :: Integer -> Integer
sumOfSquares lastNumber = sum $ map square [1..lastNumber]


squareOfSum :: Integer -> Integer
squareOfSum lastNumber = (square . sum) [1..lastNumber]

solution :: Integer
solution = squareOfSum 100 - sumOfSquares 100

-- Answer: 25164150
-- Found: 25164150