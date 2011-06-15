module Main where

import qualified Data.Vector as V
import Graphics.Gloss.Interface.Simulate
import Life

initWorld = newWorld' 5 (V.fromList . map Cell $ cells)
    where cells = [False, True,  False, False, False,
                   False, False, True,  False, False,
                   True,  True,  True, False, False,
                   False, False, False, False, False,
                   False, False, False, False, False]
                   

main = simulateInWindow "Life"
       (500, 500)
       (10, 10)
       white
       30
       initWorld
       (scale 10 10 . renderWorld)
       (\_ _ world -> stepWorld world)
       
