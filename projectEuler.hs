-- Problem One: Multiples of 3 and 5
problemOne x = sum [y | y <- (take (x - 1) [1..]), y `mod` 3 == 0 || y `mod` 5 == 0]

-- Problem Two: Even Fibonacci Numbers
getFib x = if x < 2 then x else getFib (x - 1) + getFib (x - 2)
fib = [getFib x | x <- [0..]]
fib4mil = takeWhile (\x -> x < 4000000) fib
problemTwo = sum [x | x <- fib4mil, x `mod` 2 == 0]

-- Problem Three: Largest Prime Factor
-- TODO: Make this not suk and actually work
factors x = [y | y <- [1..x], x `mod` y == 0]
isPrime x = factors x == [1, x]
primes = [x | x <- [1..], isPrime x]
primeFactors x = [y | y <- takeWhile (\y -> y <= x `div` 2) primes, x `mod` y == 0]
problemThree = last (primeFactors 600851475143)
