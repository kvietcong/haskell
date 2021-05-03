import Data.Maybe (fromJust, isJust)


----------------------
-- My First Attempt --
----------------------

operators' :: [(String, Float -> Float -> Float)]
operators' = [ ("+", (+))
             , ("*", (*))
             , ("-", (-))
             , ("/", (/))]

reversePolish' :: [String] -> [String]
reversePolish' x
    | length x > 2 =
        if isJust opLookup
           then reversePolish' $ operated:xs
           else reversePolish' $ a:reversePolish' (b:op:xs)
    | otherwise = x
    where a = head x
          b = x !! 1
          op = x !! 2
          xs = drop 3 x
          opLookup = lookup op operators'
          operator = fromJust opLookup
          operated = show $ operator (read a) (read b)

reversePolish'' :: String -> Float
reversePolish'' = read . head . reversePolish' . words


-----------------------------------------------------------------------
--                        My Second Attempt                          --
-- After looking at and understanding Learn You a Haskell's solution --
-----------------------------------------------------------------------

-- All available operators
operators :: [(String, Either (Float -> Float -> Float) ([Float] -> Float))]
operators = [ ("+", Left (+))
            , ("*", Left (*))
            , ("-", Left (-))
            , ("/", Left (/))
            , ("^", Left (**))
            , ("sum", Right sum)
            , ("min", Right minimum)
            , ("max", Right maximum)
            , ("avg", Right (\xs -> sum xs / (fromIntegral . length) xs))]

reversePolish :: String -> Float
reversePolish = head . foldl reversePolish [] . words
    where reversePolish nums next
            -- If the next value is an operator, then operate
            | isJust opLookup = case operator of
                                  Left op -> op y x:ys
                                  Right op -> [op nums]
            -- If the next value is just a value, add it onto
            -- the "stack"
            | otherwise = read next:nums
            where opLookup = lookup next operators
                  operator = fromJust opLookup
                  x = head nums
                  y = nums !! 1
                  ys = drop 2 nums

-- Learn you a Haskell's solution
-- When this clicked in my mind, my third eye twitched
-- True understanding of Monads will let me see the true light XD
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs

reversePolishPrompt = do
    putStrLn "\nPlease enter your reverse polish statement (space delimiters)"
    polish <- getLine

    print $ reversePolish polish

    putStrLn "Continue? [y/n]"
    continue <- getLine
    if continue `elem` ["yes", "y"]
       then reversePolishPrompt
       else putStrLn "Bye!"

main = do
    putStrLn "Welcome to a bad Reverse Polish Calculator"
    reversePolishPrompt
