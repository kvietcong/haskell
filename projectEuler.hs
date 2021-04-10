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
smallestMultiple' :: Integral a => a -> a -> a
smallestMultiple' x y = head $ take 1 [a |
    a <- [y,2*y..], all (\x -> a `mod` x == 0) [x..y]]

-- Maybe use this for later?
primeExponents :: Integral a => a -> [(a, Int)]
primeExponents x = [(head y, length y) | y <- group $ primeFactors x]

