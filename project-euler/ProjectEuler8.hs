import Data.Char (digitToInt)
{-
    The four adjacent digits in the $1000$-digit number that have the greatest product are $9 \times 9 \times 8 \times 9 = 5832$.</p>

73167176531330624919225119674426574742355349194934<br>
96983520312774506326239578318016984801869478851843<br>
85861560789112949495459501737958331952853208805511<br>
12540698747158523863050715693290963295227443043557<br>
66896648950445244523161731856403098711121722383113<br>
62229893423380308135336276614282806444486645238749<br>
30358907296290491560440772390713810515859307960866<br>
70172427121883998797908792274921901699720888093776<br>
65727333001053367881220235421809751254540594752243<br>
52584907711670556013604839586446706324415722155397<br>
53697817977846174064955149290862569321978468622482<br>
83972241375657056057490261407972968652414535100474<br>
82166370484403199890008895243450658541227588666881<br>
16427171479924442928230863465674813919123162824586<br>
17866458359124566529476545682848912883142607690042<br>
24219022671055626321111109370544217506941658960408<br>
07198403850962455444362981230987879927244284909188<br>
84580156166097919133875499200524063689912560717606<br>
05886116467109405077541002256983155200055935729725<br>
71636269561882670428252483600823257530420752963450<br></p>
Find the thirteen adjacent digits in the $1000$-digit number that have the greatest product. What is the value of this product?
-}

-- >>> all2AdjacentsString ""
-- []
-- >>> all2AdjacentsString "a"
-- []

-- >>> all2AdjacentsString "ab"
-- ["ab"]

-- >>> all2AdjacentsString "aba"
-- ["ab","ba"]

-- >>> all2AdjacentsString "abac",
-- ["ab","ba","ac"]

-- >>> all2AdjacentsString "abacd"
-- ["ab","ba","ac","cd"]

-- >>> all2AdjacentsString "abacde"
-- ["ab","ba","ac","cd","de"]


-- >>> all2AdjacentsString "aaa"
-- ["aa","aa"]


all2AdjacentsString :: [Char] -> [[Char]]
all2AdjacentsString [] = []
all2AdjacentsString [x] = []
all2AdjacentsString [x, y] = [[x, y]]
all2AdjacentsString (fst:snd:restWithThirdAsFirst@(third:rest)) = [[fst, snd]] ++ [[snd, third]] ++ all2AdjacentsString restWithThirdAsFirst


allNAdjacentsStrings :: Int -> [Char] -> [[Char]]
allNAdjacentsStrings x [] = []
allNAdjacentsStrings i x
    | length x < i = []
    | length x == i = [x]
    | otherwise = take i x : allNAdjacentsStrings i restWithNextCharAsFirst
    where
        restWithNextCharAsFirst = drop 1 x




all1000DigitsNumbers :: [Integer]
all1000DigitsNumbers = map read allNumbersWith1000Digits
    where
        allNumbersWith1000OrMoreDigits = map show [10^999..]
        allNumbersWith1000Digits = takeWhile ((== 1000) . length) allNumbersWith1000OrMoreDigits


-- >>> allNAdjacentsStrings 3 ""
-- []

-- >>> allNAdjacentsStrings 3 "a"
-- []

-- >>> allNAdjacentsStrings 4 "abc"
-- []

-- >>> allNAdjacentsStrings 3 "abc"
-- ["abc"]

-- >>> allNAdjacentsStrings 2 "abc"
-- ["ab","bc"]


-- >>> allNAdjacentsStrings 2 "abcd"
-- ["ab","bc","cd"]

-- >>> allNAdjacentsStrings 2 "abcde"
-- ["ab","bc","cd","de"]

-- >>> allNAdjacentsStrings 3 "abcde"
-- ["abc","bcd","cde"]

-- >>> allNAdjacentsStrings 4 "abcde"
-- ["abcd","bcde"]

-- >>> allNAdjacentsStrings 4 "abcde"
-- ["abcd","bcde"]


someNumber :: Integer
someNumber = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

q :: Int
q = maximum $ map toProduct $ allNAdjacentsStrings 4 (show someNumber)

-- >>> q
-- 5832

toProduct :: String -> Int
toProduct "" = 1
toProduct (fst:s) = (digitToInt fst :: Int) * toProduct s

-- >>> toProduct ""
-- 1

-- >>> toProduct "2"
-- 1


solution :: Int
solution = maximum $ map ((maximum . map toProduct . allNAdjacentsStrings 13) . show) all1000DigitsNumbers

-- >>> solution

x = map (allNAdjacentsStrings 13 . show) all1000DigitsNumbers


z :: Int
z = (maximum . map toProduct) $ allNAdjacentsStrings 2 "123"

-- >>> z
-- 6
