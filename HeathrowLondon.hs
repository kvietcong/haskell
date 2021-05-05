import Control.Monad (sequence)

data Choice = Choice Int Int Int deriving Show
data Side = Top Int | Bottom Int deriving Show

inputs = [[0    , 50    , 10 ]
         ,[30   , 5     , 90 ]
         ,[20   , 40    , 2  ]
         ,[25   , 10    , 8  ]]

choices = map inputToChoice inputs
    where inputToChoice input = let [a, b, c] = input in Choice a b c

path = tail . reverse $ choose [Top 0] choices
    where choose steps [] = steps
          choose steps (Choice cross top bot:choices) =
              case step of
                Top a -> if top < crossBot
                             then choose (Top top:steps) choices
                             else choose (Bottom crossBot:steps) choices
                Bottom a -> if bot < crossTop
                             then choose (Bottom bot:steps) choices
                             else choose (Top crossTop:steps) choices
                where step = head steps
                      crossTop = top + cross
                      crossBot = bot + cross

price = sum $ map fromSide path
    where fromSide side = case side of
                            Top a -> a
                            Bottom b -> b
