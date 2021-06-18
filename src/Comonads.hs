{- 
    Resources I used to learn
    - https://www.youtube.com/watch?v=6eiS2QTQKPE
    - https://eli-jordan.github.io/2018/02/16/life-is-a-comonad/
    - https://stackoverflow.com/questions/42905514/nested-list-haskell-iteration
    
    NOTE: I have improved the code and put in in its own repo
    @ https://github.com/kvietcong/comonadic-life
-}

import Control.Comonad
import Data.List (intercalate)
import System.Process (system)
import Control.Concurrent (threadDelay)

data Element = Fire
             | Water
             | Air
             | Earth
             deriving (Eq, Enum)

instance Show Element where
    show element
      | element == Fire = "F"
      | element == Water = "W"
      | element == Air = "A"
      | element == Earth = "E"
      | otherwise = "?"

data Cell = Alive | Dead deriving (Eq, Enum)

instance Show Cell where show cell 
                            | cell == Alive = "O"
                            | otherwise = "·"

data Zipper a = Zipper [a] a [a] deriving Eq

instance Functor Zipper where
    fmap function (Zipper left mid right) = Zipper
        (function <$> left)
        (function mid)
        (function <$> right)

instance Show a => Show (Zipper a) where
    show (Zipper left mid right) = (show . reverse) left
                                ++ " " ++ show mid ++ " "
                                ++ show right

shiftRight :: Zipper a -> Zipper a
shiftRight (Zipper left mid (r:rs)) = Zipper (mid:left) r rs
shiftRight zipper@(Zipper _ _ []) = zipper

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zipper (l:ls) mid right) = Zipper ls l (mid:right)
shiftLeft zipper@(Zipper [] _ _) = zipper

instance Comonad Zipper where
    extract (Zipper _ mid _) = mid
    duplicate zipper@(Zipper left _ right) = Zipper lefts zipper rights
        where lefts = tail $ scanl (\z _ -> shiftLeft z) zipper left
              rights = tail $ scanl (\z _ -> shiftRight z) zipper right

newtype Zipper2D a = Zipper2D (Zipper (Zipper a)) deriving Eq

instance Functor Zipper2D where
    -- Used to apply a function throughout the whole 2D zipper
    fmap function (Zipper2D zipper2D) = Zipper2D $
        (fmap . fmap) function zipper2D

instance Show a => Show (Zipper2D a) where
    show (Zipper2D (Zipper upZips (Zipper left mid right) downZips)) =
        intercalate "\n" [
            intercalate "\n" ((map show . reverse) upZips),
            (show . reverse) left ++ "{" ++ show mid ++ "}" ++ show right,
            intercalate "\n" (map show downZips)]

instance Comonad Zipper2D where
    extract (Zipper2D zipper2D) = extract . extract $ zipper2D
    -- My Interpretation of duplicate:
    -- First, you get the original nested zipper and apply dupe.
    -- This will nest the zipper2D in one more layer of Zipper.
    -- Dupe has the same concept as duplicate for Zipper but instead
    -- of shifting right or left on the outer layer, we know
    -- that zipper2D will need to be shifted one layer deeper.
    -- (This can be seen with the checks and the fmaps)
    -- Then you need to apply dupe again put another Zipper Layer
    -- on. Now you end up with a 4 Zipper layer monster. We
    -- can now use the Zipper2D constructor to get rid of
    -- the outer two Zipper layers. Finally using fmap, we
    -- can convert the last 2 Zipper layers into another
    -- Zipper2D and make a double layer Zipper2D
    duplicate (Zipper2D zipper2D) = Zipper2D <$> Zipper2D ((dupe . dupe) zipper2D)
        where dupe zipper = Zipper (lefts zipper) zipper (rights zipper)
              lefts zipper@(Zipper _ (Zipper left _ _) _) =
                  tail $ scanl (\z _ -> shiftLeft <$> z) zipper left
              rights zipper@(Zipper _ (Zipper _ _ right) _) =
                  tail $ scanl (\z _ -> shiftRight <$> z) zipper right

goUp :: Zipper2D a -> Zipper2D a
goUp (Zipper2D zipper2D) = Zipper2D $ shiftLeft zipper2D

goDown :: Zipper2D a -> Zipper2D a
goDown (Zipper2D zipper2D) = Zipper2D $ shiftRight zipper2D

goLeft :: Zipper2D a -> Zipper2D a
goLeft (Zipper2D zipper2D) = Zipper2D $ shiftLeft <$> zipper2D

goRight :: Zipper2D a -> Zipper2D a
goRight (Zipper2D zipper2D) = Zipper2D $ shiftRight <$> zipper2D

getNeighbors :: Zipper2D a -> [a]
getNeighbors zipper2D@(Zipper2D (Zipper up (Zipper left _ right) down)) = extract <$> neighbors
  where canGoLeft = not . null $ left
        canGoRight = not . null $ right
        canGoUp = not . null $ up
        canGoDown = not . null $ down
        neighbors = [goLeft zipper2D | canGoLeft]
                 ++ [goLeft . goUp $ zipper2D | canGoLeft && canGoUp]
                 ++ [goLeft . goDown $ zipper2D | canGoLeft && canGoDown]
                 ++ [goRight zipper2D | canGoRight]
                 ++ [goRight . goUp $ zipper2D | canGoRight && canGoUp]
                 ++ [goRight . goDown $ zipper2D | canGoRight && canGoDown]
                 ++ [goUp zipper2D | canGoUp]
                 ++ [goDown zipper2D | canGoDown]

exampleInts :: Zipper2D Int
exampleInts = Zipper2D (Zipper
                        [Zipper [2,1] 6 [2,1],
                         Zipper [1,1] 2 [1,1]]
                        (Zipper [6,2] 0 [6,2])
                        [Zipper [2,1] 6 [2,1],
                         Zipper [1,1] 2 [1,1]]
                       )

exampleCells' :: Zipper2D Cell
exampleCells' = Zipper2D (Zipper
                          [Zipper [Dead,Dead] Alive [Dead,Dead],
                           Zipper [Dead,Dead] Dead [Dead,Dead]]
                          (Zipper [Dead,Dead] Alive [Dead,Dead])
                          [Zipper [Dead,Dead] Alive [Dead,Dead],
                           Zipper [Dead,Dead] Dead [Dead,Dead]]
                         )

infiCellAlt :: [Cell]
infiCellAlt = [Alive,Dead] ++ infiCellAlt

infiDeadRows :: [Zipper Cell]
infiDeadRows = repeat $ Zipper (repeat Dead) Dead (repeat Dead)

exampleCells :: Zipper2D Cell
exampleCells = Zipper2D (Zipper rows (head rows) rows)
    where cells = take 36 infiCellAlt
          rows = replicate 12 $ Zipper cells Alive cells

exampleElements :: Zipper2D Element
exampleElements = Zipper2D (Zipper
                            [Zipper [Water,Air]   Fire    [Water,Air],
                             Zipper [Air,Air]     Air     [Air,Air]]
                            (Zipper [Water,Air]   Earth   [Water,Air])
                            [Zipper [Water,Air]   Fire    [Water,Air],
                             Zipper [Air,Air]     Air     [Air,Air]]
                           )

class Display a where
    display :: a -> String

instance Show a => Display (Zipper a) where
    display (Zipper left mid right) = concat ((map show . reverse) left
                                   ++ [show mid]
                                   ++ map show right)

instance Show a => Display (Zipper2D a) where
    display (Zipper2D (Zipper upZips midZip downZips)) =
        intercalate "\n" [
            intercalate "\n" ((map display . reverse) upZips),
            display midZip,
            intercalate "\n" (map display downZips)]

elementalRules :: Zipper2D Element -> Element
elementalRules elements = case extract elements of
                            Air -> air neighbors
                            Fire -> fire neighbors
                            Earth -> earth neighbors
                            Water -> water neighbors
    where neighbors = getNeighbors elements
          getElementAmount element = length . filter (==element)
          air neighbors = if getElementAmount Fire neighbors > 2
                             then Water
                             else Air
          fire neighbors = if getElementAmount Water neighbors > 2
                              then Water
                              else Fire
          earth neighbors = if getElementAmount Water neighbors > 4
                              then Water
                              else Earth
          water neighbors = if getElementAmount Fire neighbors > 4
                               then Air
                               else Water

elementalCraft :: Zipper2D Element -> [Zipper2D Element]
elementalCraft = iterate (extend elementalRules)

gameOfLifeRules :: Zipper2D Cell -> Cell
gameOfLifeRules cells = case cell of
                          Dead -> if alive == 3 then Alive else Dead
                          Alive -> if alive `elem` [2,3] then Alive else Dead
    where cell = extract cells
          neighbors = getNeighbors cells
          alive = length . filter (==Alive) $ neighbors
          dead = length . filter (==Dead) $ neighbors

gameOfLife :: Zipper2D Cell -> [Zipper2D Cell]
gameOfLife = iterate (extend gameOfLifeRules)

microSecondsInSecond :: Int
microSecondsInSecond = 1000000

customAnimate :: Display a => Int -> [a] -> (a -> String) -> IO ()
customAnimate delay states displayFunction = do
    mapM_
        (\state -> do
            putStrLn $ displayFunction state
            threadDelay delay
            system "cls"
        ) states

animate :: Display a => Int -> [a] -> IO ()
animate delay states = customAnimate delay states display

main :: IO ()
main = do
    system "cls"
    animate (microSecondsInSecond `div` 3) (gameOfLife exampleCells)
