import Data.Maybe (fromJust, isJust)
import Data.List (foldl')


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

-- Operator type with three types of Arity
data Operator unary binary nary = Unary unary | Binary binary | Nary nary

-- All available operators for the program
operators :: [( String
              , Operator 
                    (Float -> Float) 
                    (Float -> Float -> Float)
                    ([Float] -> Float)
              )]
operators = [ ("log", Unary log)
            , ("sqrt", Unary sqrt)
            , ("exp", Unary exp)
            , ("sin", Unary sin)
            , ("cos", Unary cos)
            , ("tan", Unary tan)
            , ("sinh", Unary sinh)
            , ("cosh", Unary cosh)
            , ("tanh", Unary tanh)
            , ("round", Unary $ fromIntegral . round)
            , ("ceiling", Unary $ fromIntegral . ceiling)
            , ("floor", Unary $ fromIntegral . floor)

            , ("+", Binary (+))
            , ("*", Binary (*))
            , ("-", Binary (-))
            , ("/", Binary (/))
            , ("^", Binary (**))

            , ("sum", Nary sum)
            , ("min", Nary minimum)
            , ("max", Nary maximum)
            , ("avg", Nary (\xs -> sum xs / (fromIntegral . length) xs))]

polish :: Bool -> String -> Float
polish reversed
    | reversed = head . foldl' (flip polish) [] . words
    | otherwise = head . foldr polish [] . words
    where polish next nums
            -- If the next value is an operator, then operate
            | isJust opLookup = case operator of
                                  Unary op -> op x:xs
                                  Binary op -> if reversed 
                                                  then op y x:ys 
                                                  else op x y:ys
                                  Nary op -> [op nums]
            -- If the next value is just a value, add it onto
            -- the "stack"
            | otherwise = read next:nums
            where opLookup = lookup next operators
                  operator = fromJust opLookup
                  x = head nums
                  xs = drop 1 nums
                  y = nums !! 1
                  ys = drop 2 nums

-- Learn you a Haskell's solution
-- When this clicked in my mind, my third eye twitched
-- True understanding of Monads will let me see the true light XD
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
  where foldingFunction (x:y:ys) "*" = (x * y):ys  
        foldingFunction (x:y:ys) "+" = (x + y):ys  
        foldingFunction (x:y:ys) "-" = (y - x):ys  
        foldingFunction (x:y:ys) "/" = (y / x):ys  
        foldingFunction (x:y:ys) "^" = (y ** x):ys  
        foldingFunction (x:xs) "ln" = log x:xs  
        foldingFunction xs "sum" = [sum xs]  
        foldingFunction xs numberString = read numberString:xs

polishStatement :: Bool -> String
polishStatement reversed
    | reversed = '\n':"Please enter your Reverse Polish statement (space delimiters)"
    | otherwise = '\n':"Please enter your Polish statement (space delimiters)"

polishPrompt = do
    putStrLn $ '\n':"Polish or Reverse Polish? (p for Polish)"
    choice <- getLine
    let reversed = choice /= "p"

    putStrLn $ polishStatement reversed
    statement <- getLine

    print $ polish reversed statement

    putStrLn $ '\n':"Continue? [y/n]"
    continue <- getLine
    if continue `elem` ["yes", "y"]
       then polishPrompt
       else putStrLn $ '\n':"Bye!"

main = do
    putStrLn "Welcome to my bad Polish Notation Solver"
    polishPrompt
