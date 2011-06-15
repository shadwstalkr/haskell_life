module Main where

import Graphics.Gloss.Interface.Simulate
import Life

makeCells :: Int -> [Bool] -> [Cell]
makeCells width cells = makeCells' 0 0 cells
    where makeCells' _ _ [] = []
          makeCells' x y (live:rs) = Cell (x, y) live : makeCells' newx newy rs
              where nextrow = x == (width - 1)
                    newx = if nextrow then 0 else x + 1
                    newy = if nextrow then y + 1 else y

initWorld = newWorld' 5 (makeCells 5 cells)
    where cells = [False, True,  False, False, False,
                   False, False, True,  False, False,
                   True,  True,  True, False, False,
                   False, False, False, False, False,
                   False, False, False, False, False]
                   

main = simulateInWindow "Life" (500, 500) (10, 10) white 1 initWorld renderWorld (\_ _ world -> stepWorld world)
       
