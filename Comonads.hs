-- A lot of help from: https://stackoverflow.com/questions/42905514/nested-list-haskell-iteration
import Control.Monad
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

data Cell = Alive | Dead deriving Eq

instance Show Cell where
    show cell 
      | cell == Alive = "O"
      | cell == Dead  = "."

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
    duplicate zipper = Zipper lefts zipper rights
        where lefts = convergeLeft zipper []
              rights = convergeRight zipper []
              convergeLeft zipper@(Zipper left _ _) accumulated =
                  if null left
                     then accumulated
                     else newZip:convergeLeft newZip accumulated
                  where newZip = shiftLeft zipper
              convergeRight zipper@(Zipper _ _ right) accumulated =
                  if null right
                     then accumulated
                     else newZip:convergeRight newZip accumulated
                  where newZip = shiftRight zipper
    extend function = fmap function . duplicate

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
    -- Okay, I still am confused as to how this totally worked. I just made the
    -- types line up and it works LOL
    duplicate (Zipper2D zipper2D) = Zipper2D <$> (Zipper2D . dupe . dupe) zipper2D
        where dupe zipper = Zipper (lefts zipper) zipper (rights zipper)
              lefts zipper = convergeLeft zipper []
              rights zipper = convergeRight zipper []
              convergeLeft zipper@(Zipper _ (Zipper innerLefts _ _) _) accumulated =
                  if null innerLefts
                     then accumulated
                     else newZip:convergeLeft newZip accumulated
                  where newZip = shiftLeft <$> zipper
              convergeRight zipper@(Zipper _ (Zipper _ _ innerRights) _) accumulated =
                  if null innerRights
                     then accumulated
                     else newZip:convergeRight newZip accumulated
                  where newZip = shiftRight <$> zipper

goUp :: Zipper2D a -> Zipper2D a
goUp (Zipper2D zipper2D) = Zipper2D $ shiftLeft zipper2D

goDown :: Zipper2D a -> Zipper2D a
goDown (Zipper2D zipper2D) = Zipper2D $ shiftRight zipper2D

goLeft :: Zipper2D a -> Zipper2D a
goLeft (Zipper2D zipper2D) = Zipper2D $ fmap shiftLeft zipper2D

goRight :: Zipper2D a -> Zipper2D a
goRight (Zipper2D zipper2D) = Zipper2D $ fmap shiftRight zipper2D

-- Fix this so that you don't need Eq constraint
getNeighbors :: Eq a => Zipper2D a -> [a]
getNeighbors zipper2D = extract <$> filter (/= zipper2D) direct
                                 ++ filter (`notElem` [goUp zipper2D
                                                      , goDown zipper2D
                                                      , goLeft zipper2D
                                                      , goRight zipper2D]) diagonals
  where direct = [ goUp zipper2D
                 , goLeft zipper2D
                 , goRight zipper2D
                 , goDown zipper2D]
        diagonals = [ goUp . goLeft $ zipper2D
                    , goUp . goRight $ zipper2D
                    , goDown . goLeft $ zipper2D
                    , goDown . goRight $ zipper2D]

sumOfNeighbors' :: Num a => Zipper a -> a
sumOfNeighbors' (Zipper (l:_) _ (r:_)) = l + r

balancedZip :: Zipper Int
balancedZip = Zipper [-1,-2..(-5)] 0 [1..5]

leftHeavyZip :: Zipper Int
leftHeavyZip = Zipper [4,3..(-5)] 5 []

rightHeavyZip :: Zipper Int
rightHeavyZip = Zipper [] (-5) [-4..5]

testInts :: Zipper2D Int
testInts = Zipper2D (Zipper
                     [Zipper [2,1] 6 [2,1],
                      Zipper [1,1] 2 [1,1]]
                     (Zipper [6,2] 0 [6,2])
                     [Zipper [2,1] 6 [2,1],
                      Zipper [1,1] 2 [1,1]]
                    )

testCells :: Zipper2D Cell
testCells = Zipper2D (Zipper
                     [Zipper [Alive,Dead] Alive [Alive,Alive],
                      Zipper [Dead,Dead] Dead [Dead,Alive]]
                     (Zipper [Alive,Dead] Dead [Alive,Dead])
                     [Zipper [Dead,Alive] Dead [Alive,Dead],
                      Zipper [Alive,Alive] Dead [Alive,Alive]]
                    )

testElements :: Zipper2D Element
testElements = Zipper2D (Zipper
                         [Zipper [Water,Air]   Fire    [Water,Air],
                          Zipper [Air,Air]     Air     [Air,Air]]
                         (Zipper [Water,Air]   Earth   [Water,Air])
                         [Zipper [Water,Air]   Fire    [Water,Air],
                          Zipper [Air,Air]     Air     [Air,Air]]
                        )

elementalRule :: Zipper2D Element -> Element
elementalRule zipper2D = case extract zipper2D of
                           Air -> air neighbors
                           Fire -> fire neighbors
                           Earth -> earth neighbors
                           Water -> water neighbors
    where neighbors = getNeighbors zipper2D
          getElementAmount element = length . filter (==element)
          air neighbors = if getElementAmount Fire neighbors > 3
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

elementalCraft :: Zipper2D Element -> [Zipper2D Element]
elementalCraft = iterate (extend elementalRule)

displayItem :: Display a => Int -> a -> IO ()
displayItem delay item = do
    threadDelay delay
    system "cls"
    putStrLn $ display item

microSecondsInSecond :: Int
microSecondsInSecond = 1000000

printDelay :: Int
printDelay = microSecondsInSecond

main :: IO ()
main = do
    system "cls"
    mapM_ (displayItem printDelay) (take 10 $ elementalCraft testElements)
    print "Done"
