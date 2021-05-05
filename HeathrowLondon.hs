data Section = Section Int Int Int deriving Show
data Choice = Top Int | Cross Int | Bottom Int deriving Show

fromSide :: Choice -> Int
fromSide side = case side of
                  Top a -> a
                  Cross a -> a
                  Bottom b -> b

roadSystem :: [Section]
roadSystem = [ Section 0  50 10 
             , Section 30 5  90 
             , Section 20 40 2  
             , Section 25 10 8 ]

path :: [Choice]
path = dropWhile ((== 0) . fromSide) $ (reverse . choose [Top 0]) roadSystem
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

price :: Int
price = sum $ fmap fromSide path

main :: IO ()
main = do
    putStrLn "This is the Road System"
    print roadSystem
    putStrLn $ '\n':"This is the Path"
    print path
    putStrLn $ '\n':"This is the Price"
    print price

{-
------------------------------------
-- Learn You a Haskell's Solution --
------------------------------------
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
