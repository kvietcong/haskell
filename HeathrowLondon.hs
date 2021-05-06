{-|
   Data structure that represents the price to choose the
   cross, top, or bottom path (In that order respectively)
-}
data Section = Section { crossPrice :: Int
                       , topPrice :: Int
                       , bottomPrice :: Int }
                       deriving Show

{-|
   Data structure that represents the choice taken when at
   a section and its associated cost
-}
data Choice =  Cross Int | Top Int | Bottom Int deriving Show

{-|
   Unwraps a Choice value into the amount of
   effort it took for that choice
-}
fromChoice :: Choice -> Int
fromChoice side = case side of
                    Cross a -> a
                    Top a -> a
                    Bottom a -> a

{-|
   The Heathrow to London Road System found
   in the Functionally Solving Problems chapter of
   the Learn You a Haskell Book
-}
roadSystem :: [Section]
roadSystem = [ Section 0  50 10 
             , Section 30 5  90 
             , Section 20 40 2  
             , Section 25 10 8 ]

-- |Retrives the best choices to minimize the price associated
bestPath :: [Section] -> [Choice]
bestPath = dropWhile ((== 0) . fromChoice) . reverse . choose [Top 0]
    where choose choices [] = choices
          choose choices (Section cross top bot:sections) =
              case head choices of
                Top _ -> if top < cross + bot
                            then choose (Top top:choices) sections
                            else choose (Bottom bot:Cross cross:choices) sections
                Bottom _ -> if bot < cross + top
                               then choose (Bottom bot:choices) sections
                               else choose (Top top:Cross cross:choices) sections
                Cross _ -> error "This shouldn't happen"

-- |Retrieves the total price of a given set of choices
price :: [Choice] -> Int
price = sum . fmap fromChoice

-- |Entry point that prints out the solutions
main :: IO ()
main = do
    let path = bestPath roadSystem

    putStrLn $ "The Road System: " ++ show roadSystem

    putStrLn $ '\n':"The best path is: " ++ show path

    putStrLn $ '\n':"This is the best price: " ++
        (show . price) path ++ " units"

------------------------------------
-- Learn You a Haskell's Solution --
------------------------------------
{-
data Section' = Section' { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section']  

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section' 50 10 30, Section' 5 90 20, Section' 40 2 25, Section' 10 8 0]

roadStep :: (Path, Path) -> Section' -> (Path, Path)  
roadStep (pathA, pathB) (Section' a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath

pathPrice :: Int
pathPrice = sum $ map snd $ optimalPath heathrowToLondon
-}
