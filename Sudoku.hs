type Grid a = [[a]]

printGrid :: Show a => Grid a -> IO ()
printGrid grid = do
    mapM_ print grid

isEmpty :: Int -> Bool
isEmpty = (==0)

possibleChoices :: [Int]
possibleChoices = [1..9]

isDecided :: [a] -> Bool
isDecided [_] = True
isDecided _   = False

easy :: Grid Int
easy = [[2,0,0,0,0,1,0,3,8],
        [0,0,0,0,0,0,0,0,5],
        [0,7,0,0,0,6,0,0,0],
        [0,0,0,0,0,0,0,1,3],
        [0,9,8,1,0,0,2,5,7],
        [3,1,0,0,0,0,8,0,0],
        [9,0,0,8,0,0,0,2,0],
        [0,5,0,0,6,9,7,8,4],
        [4,0,0,2,5,0,0,0,0]]

gentle :: Grid Int
gentle = [[0,1,0,4,2,0,0,0,5],
          [0,0,2,0,7,1,0,3,9],
          [0,0,0,0,0,0,0,4,0],
          [2,0,7,1,0,0,0,0,6],
          [0,0,0,0,4,0,0,0,0],
          [6,0,0,0,0,7,4,0,3],
          [0,7,0,0,0,0,0,0,0],
          [1,2,0,7,3,0,5,0,0],
          [3,0,0,0,8,2,0,7,0]]

diabolical :: Grid Int
diabolical = [[0,9,0,7,0,0,8,6,0],
              [0,3,1,0,0,5,0,2,0],
              [8,0,6,0,0,0,0,0,0],
              [0,0,7,0,5,0,0,0,6],
              [0,0,0,3,0,7,0,0,0],
              [5,0,0,0,1,0,7,0,0],
              [0,0,0,0,0,0,1,0,9],
              [0,2,0,6,0,0,3,5,0],
              [0,5,4,0,0,8,0,7,0]]

unsolvable :: Grid Int
unsolvable = [[1,0,0,9,0,7,0,0,3],
              [0,8,0,0,0,0,0,7,0],
              [0,0,9,0,0,0,6,0,0],
              [0,0,7,2,0,9,4,0,0],
              [4,1,0,0,0,0,0,9,5],
              [0,0,8,5,0,4,3,0,0],
              [0,0,3,0,0,0,7,0,0],
              [0,5,0,0,0,0,0,4,0],
              [2,0,0,8,0,6,0,0,9]]

minimal :: Grid Int
minimal = [[0,9,8,0,0,0,0,0,0],
           [0,0,0,0,7,0,0,0,0],
           [0,0,0,0,1,5,0,0,0],
           [1,0,0,0,0,0,0,0,0],
           [0,0,0,2,0,0,0,0,9],
           [0,0,0,9,0,6,0,8,2],
           [0,0,0,0,0,0,0,3,0],
           [5,0,1,0,0,0,0,0,0],
           [0,0,0,4,0,0,0,2,0]]

blank :: Grid Int
blank = replicate 9 (replicate 9 0)

getChoiceGrid :: Grid Int -> Grid [Int]
getChoiceGrid = map $ map fillChoices
    where fillChoices x = if isEmpty x then possibleChoices else [x]

gridsFromChoices :: Grid [Int] -> [Grid Int]
gridsFromChoices = mapM sequence

solveSudoku :: Grid Int -> [Grid Int]
solveSudoku grid = undefined
