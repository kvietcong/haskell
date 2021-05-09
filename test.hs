import Data.Maybe (isJust, fromJust)

doubleMe x = 2 * x
doubleUs x y = doubleMe x + doubleMe y

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "You okay?"
maximum' [x] = x
maximum' (x:y) = max x (maximum' y)

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "You okay?"
maximum'' [x] = x
maximum'' (h:t)
    | h > maximum'' t = h
    | otherwise = maximum'' t

toSet :: Eq a => [a] -> [a]
toSet [] = []
toSet (x:xs)
  | x `elem` x' = x'
  | otherwise = x:x'
  where x' = toSet xs

uniques :: Eq a => [a] -> [a]
uniques [] = []
uniques (x:xs)
  | x `elem` x' = filter (/=x) x'
  | otherwise = x:x'
  where x' = uniques xs

sayHello :: String -> String
sayHello = (++) "Hello "

sayLame :: String -> String
sayLame = (++) "You're Lame "

isLame :: String -> Bool
isLame x = x `elem` ["KV", "KV Le"]

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort less ++ [x] ++ quickSort greater
    where less = filter (<=x) xs
          greater = filter (>x) xs

concatList' :: [[a]] -> [a]
concatList' xss = [x | xs <- xss, x <- xs]

flatCounter :: (Ord a) => [[a]] -> [(Int, a)]
flatCounter list = foldl f [] $ flatSort list
    where flatSort = quickSort . concatList'
          f [] next = [(1, next)]
          f (x:xs) next = if snd x == next
                             then (fst x + 1, next):xs
                             else (1, next):x:xs

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    if isLame name 
       then putStrLn $ sayLame name
       else putStrLn $ sayHello name

    putStrLn "Continue? [y/n]"
    continue <- getChar
    if continue == 'y'
       then main
       else pure ()
