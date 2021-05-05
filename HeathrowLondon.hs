import Control.Monad (sequence)

data Choice = Choice Int Int Int deriving Show
data Side = Top Int | Cross Int | Bottom Int deriving Show

fromSide side = case side of
                  Top a -> a
                  Cross a -> a
                  Bottom b -> b


inputs = [[0    , 50    , 10 ]
         ,[30   , 5     , 90 ]
         ,[20   , 40    , 2  ]
         ,[25   , 10    , 8  ]]

choices = map inputToChoice inputs
    where inputToChoice input = let [a, b, c] = input in Choice a b c

path = dropWhile (\x -> fromSide x == 0) $ reverse $ choose [Top 0] choices
    where choose steps [] = steps
          choose steps (Choice cross top bot:choices) =
              case step of
                Top a -> if top < crossBot
                             then choose (Top top:steps) choices
                             else choose (Bottom bot:Cross cross:steps) choices
                Bottom a -> if bot < crossTop
                             then choose (Bottom bot:steps) choices
                             else choose (Top top:Cross cross:steps) choices
                where step = head steps
                      crossTop = top + cross
                      crossBot = bot + cross

price = sum $ map fromSide path

------------------------------------
-- Learn You a Haskell's Solution --
------------------------------------
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]  

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
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
