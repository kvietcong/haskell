import Data.Maybe (isJust, fromJust)
import Data.Char (isSpace)

keep :: Eq a => (a -> Bool) -> [a] -> [a]
keep = filter

discard :: Eq a => (a -> Bool) -> [a] -> [a]
discard predicate = filter $ not . predicate

doubleMe :: Num a => a -> a
doubleMe x = 2 * x

doubleUs :: Num a => a -> a -> a
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
  | x `elem` x' = discard (==x) x'
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
    where less = keep (<=x) xs
          greater = keep (>x) xs

concatList' :: [[a]] -> [a]
concatList' xss = [x | xs <- xss, x <- xs]

flatCounter :: (Ord a) => [[a]] -> [(Int, a)]
flatCounter list = foldl f [] $ flatSort list
    where flatSort = quickSort . concatList'
          f [] next = [(1, next)]
          f (x:xs) next = if snd x == next
                             then (fst x + 1, next):xs
                             else (1, next):x:xs

guardTesting string char 
  | isSpace char && null string = "You have nothing and a space"
  | isSpace char = "This is a space"
  | otherwise = "LOL"

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
