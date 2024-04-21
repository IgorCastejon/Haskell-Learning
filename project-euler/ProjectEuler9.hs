{- <p>A Pythagorean triplet is a set of three natural numbers, $a \lt b \lt c$, for which,
$$a^2 + b^2 = c^2.$$</p>
<p>For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.</p>
<p>There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.<br>Find the product $abc$.</p>
-}

-- >>> take 1 pythagoreanTriples

allPythagoreanTriples :: [(Integer, Integer, Integer)]
allPythagoreanTriples = [(a,b,c) | c <- [2..], b <- [2..c-1], a <- [2..b-1], a^2 + b^2 == c^2]

solution :: Integer
solution = productTriple $ head . take 1 $ filter (\(a, b, c) -> a + b + c == 1000) allPythagoreanTriples


getPythagoreanTripleThatSumsTo1000 :: (Integer, Integer, Integer)
getPythagoreanTripleThatSumsTo1000 = head . take 1 $ filter (\(a, b, c) -> a + b + c == 1000) allPythagoreanTriples

-- >>> getPythagoreanTripleThatSumsTo1000
-- (200,375,425)

-- >>> solution
-- 31875000

-- Solution: 31875000

-- >>> take 10 pythagoreanTriples 
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(15,20,25),(7,24,25),(10,24,26),(20,21,29)]

productTriple ::  (Integer, Integer, Integer) -> Integer
productTriple (x,y,z) = x*y*z
