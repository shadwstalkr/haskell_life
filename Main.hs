module Main where

import System.Environment
import qualified Data.Vector as V
import Graphics.Gloss.Interface.Simulate
import Life

initWorld = newWorld' 5 (V.fromList . map Cell $ cells)
    where cells = [False, True,  False, False, False,
                   False, False, True,  False, False,
                   True,  True,  True, False, False,
                   False, False, False, False, False,
                   False, False, False, False, False]
                   
deserializeWorld :: String -> IO World
deserializeWorld packed = 
    let toCell ch = Cell $ ch == '1'
        valid ch = ch == '1' || ch == '0'
        cells = V.fromList . map toCell . filter valid $ packed
        width = length . takeWhile (/= '\n') $ packed
    in return $! newWorld' width cells

main = do
  args <- getArgs
  world <- if null args
           then
               return initWorld
           else
               readFile (head args) >>= deserializeWorld
  simulateInWindow "Life"
       (500, 500)
       (10, 10)
       white
       30
       world
       (scale 2 2 . renderWorld)
       (\_ _ -> stepWorld)
       
