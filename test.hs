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
