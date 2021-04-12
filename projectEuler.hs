import Data.List (find, group)

-- Problem One: Multiples of 3 and 5
problemOne :: Integral a => a -> a
problemOne x = sum [y |
    y <- take (fromIntegral x - 1) [1..],
    y `mod` 3 == 0 || y `mod` 5 == 0]

-- Problem Two: Even Fibonacci Numbers
getFib :: Integral a => a -> a
getFib x = if x < 2 then x
                    else getFib (x - 1) + getFib (x - 2)

fib :: Integral a => [a]
fib = [getFib x | x <- [0..]]

fib4mil :: Integral a => [a]
fib4mil = takeWhile (\x -> x < 4000000) fib

problemTwo :: Integral a => a
problemTwo = sum [x | x <- fib4mil, x `mod` 2 == 0]

-- Problem Three: Largest Prime Factor
primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = let factors = 2 : [3,5..n-1]
    in case find (\x -> (n `mod` x) == 0) factors of
         Just factor -> factor : primeFactors (n `div` factor)
         Nothing -> [n]

problemThree :: Integral a => a
problemThree = last (primeFactors 600851475143)

-- Problem Four: Largest palindrome product
-- This is my original attempt at reversing (It sucks 😢)
isPalindrome' :: Integral a => a -> Bool
isPalindrome' x = let y = show $ fromIntegral x in y == reverse y

-- I found this reversal algo online and it's super cool
-- quotRem calculates the quotient and remainder and puts it into a Tuple
-- It even makes use of the fact that every function is curried
-- https://stackoverflow.com/questions/26315917/decidein-haskell-if-a-number-is-or-not-a-palindrome-without-using-lists/
reverseIntegral :: Integral a => a -> a
reverseIntegral = reverseIntegral 0
    where reverseIntegral result 0 = result
          reverseIntegral result original =
              let (quotient, remainder) = original `quotRem` 10
               in reverseIntegral (10 * result + remainder) quotient

-- This is the latest with the cool reversal algo
isPalindrome :: Integral a => a -> Bool
isPalindrome x = x == reverseIntegral x

problemFour :: Integral a => a
problemFour = maximum [multiplied |
    x <- [100..999], y <- [100..999],
    let multiplied = x * y,
    isPalindrome multiplied]

-- Problem Five: Smallest Multiple
-- This solution is pretty slow tbh
-- #TODO: Find a way to do this better
smallestMultiple' :: Integral a => a -> a -> a
smallestMultiple' x y = head $ take 1 [a |
    a <- [y,2*y..], all (\x -> a `mod` x == 0) [x..y]]

-- Maybe use this for later?
primeExponents :: Integral a => a -> [(a, Int)]
primeExponents x = [(head y, length y) | y <- group $ primeFactors x]

-- Problem Six: Sum square difference
sumSquareSquareSum :: Integral a => a -> a -> (a, a)
sumSquareSquareSum x y = (sum [z * z | z <- [x..y]], let sum' = sum [x..y] in sum' * sum')

difference :: Integral a => (a, a) -> a
difference (x, y) = y - x

problemSix :: Integral a => a
problemSix = difference $ sumSquareSquareSum 1 100

-- Problem Seven: 10001st prime
-- This solution slow AF 😭
-- #TODO: Find a way to do this better
isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = null $ [y | y <- [2..x `div` 2], x `mod` y == 0]

primes :: [Int]
primes = [x | x <- 2:[3,5..], isPrime x]

problemSeven :: Int
problemSeven = primes !! 10000

-- Problem Eight: Largest product in a series
thousandDigitNumber :: [Char]
thousandDigitNumber =
    "73167176531330624919225119674426574742355349194934\
    \96983520312774506326239578318016984801869478851843\
    \85861560789112949495459501737958331952853208805511\
    \12540698747158523863050715693290963295227443043557\
    \66896648950445244523161731856403098711121722383113\
    \62229893423380308135336276614282806444486645238749\
    \30358907296290491560440772390713810515859307960866\
    \70172427121883998797908792274921901699720888093776\
    \65727333001053367881220235421809751254540594752243\
    \52584907711670556013604839586446706324415722155397\
    \53697817977846174064955149290862569321978468622482\
    \83972241375657056057490261407972968652414535100474\
    \82166370484403199890008895243450658541227588666881\
    \16427171479924442928230863465674813919123162824586\
    \17866458359124566529476545682848912883142607690042\
    \24219022671055626321111109370544217506941658960408\
    \07198403850962455444362981230987879927244284909188\
    \84580156166097919133875499200524063689912560717606\
    \05886116467109405077541002256983155200055935729725\
    \71636269561882670428252483600823257530420752963450"

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

thousandDigitNumber' :: [Int]
thousandDigitNumber' = [digitToInt x | x <- thousandDigitNumber]

slice :: Int -> Int -> [a] -> [a]
slice x y = drop x . take y


digitProduct :: Integer -> Integer
digitProduct x = product [read $ y:[] | y <- show x]

problemEight :: Integer
problemEight = maximum [digitProduct $ read number |
    x <- [0..1001-13], let number = slice x (x + 13) thousandDigitNumber]

-- Problem 9: Special Pythagorean triplet
pythagoreanTriplets :: [(Int, Int, Int)]
pythagoreanTriplets = [(a, b, c) | c <- [1..], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2]

productTriplet :: (Int, Int, Int) -> Int
productTriplet (a, b, c) = a * b * c

extractMaybe :: Maybe a -> a
extractMaybe x = case x of
                   Just x -> x
                   Nothing -> error "Nothing :("

-- #TODO: Find a way to do this better (Monads)
problemNine :: Int
problemNine = productTriplet $ extractMaybe $
    find (\(a, b, c) -> a + b + c == 1000) pythagoreanTriplets
