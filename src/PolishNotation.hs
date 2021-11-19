import Data.Maybe (fromJust, isJust)
import Data.List (foldl')

-----------------------
-- My Second Attempt --
-----------------------
-- After looking at and understanding
-- Learn You a Haskell's solution

-- |Data type that has represents three types of operators
-- Unary, Binary, and N-Nary operators (based on arity)
data Operator = Unary (Float -> Float)
              | Binary (Float -> Float -> Float)
              | Nary ([Float] -> Float)

-- Just make types more strict so that compiler doesn't give warnings
fromInt :: Int -> Float
fromInt = fromIntegral

-- |All available operators for the program
operators :: [(String, Operator)]
operators = [ ("logE", Unary log) -- log base e
            , ("log", Unary $ logBase 10)
            , ("lg", Unary $ logBase 2)
            , ("sqrt", Unary sqrt)
            , ("exp", Unary exp)
            , ("sin", Unary sin)
            , ("cos", Unary cos)
            , ("tan", Unary tan)
            , ("sinh", Unary sinh)
            , ("cosh", Unary cosh)
            , ("tanh", Unary tanh)
            , ("round", Unary $ fromInt . round)
            , ("ceiling", Unary $ fromInt . ceiling)
            , ("floor", Unary $ fromInt . floor)

            , ("+", Binary (+))
            , ("*", Binary (*))
            , ("-", Binary (-))
            , ("/", Binary (/))
            , ("^", Binary (**))
            , ("**", Binary (**))

            , ("sum", Nary sum)
            , ("min", Nary minimum)
            , ("max", Nary maximum)
            , ("avg", Nary (\xs -> sum xs / (fromIntegral . length) xs))]

-- |Given if the Polish statement is reversed or not, a
-- Polish String statement will be calculated
computePolishStatement :: Bool -> String -> Float
computePolishStatement isReversed
    | isReversed = head . foldl' (flip compute) [] . words
    | otherwise = head . foldr compute [] . words
    where compute next nums
            -- Since we know that opLookup is successful,
            -- we can extract an operation without error.
            | isJust opLookup = case operator of
                                  Unary op -> op x:xs
                                  Binary op -> if isReversed
                                                  then op y x:ys
                                                  else op x y:ys
                                  Nary op -> [op nums]
            | otherwise = read next:nums
            where opLookup = lookup next operators
                  operator = fromJust opLookup
                  x = head nums
                  xs = drop 1 nums
                  y = (head . tail) nums
                  ys = drop 2 nums

-- |Repeatable prompt for polish statement inputs
initiatePolishPrompt :: IO ()
initiatePolishPrompt = do
    putStrLn $ '\n':"Polish or Reverse Polish? [p]/r"
    isReversed <- (== "r") <$> getLine

    putStrLn $
        if isReversed
           then '\n':"Please enter your Reverse Polish statement"
           else '\n':"Please enter your Polish statement"
    putStrLn "Use Space Delimiters"

    polishStatement <- getLine
    print $ computePolishStatement isReversed polishStatement

    -- Alternative way to compute the statement
    -- result <- computePolishStatement isReversed <$> getLine
    -- print result

    putStrLn $ '\n':"Continue? [y]/n"
    willNotContinue <- flip elem ["no", "n"] <$> getLine
    if willNotContinue then putStrLn $ '\n':"Bye!" else initiatePolishPrompt

-- |Program entry point
main :: IO ()
main = do
    print "Welcome to KV's bad Polish Notation Solver"
    initiatePolishPrompt

------------------------------------
-- Learn you a Haskell's solution --
------------------------------------
{-
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
-}

----------------------
-- My First Attempt --
----------------------
-- A recursive approach that is a bit inflexible

{-
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
-}