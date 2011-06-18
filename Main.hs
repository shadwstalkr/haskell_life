{-
Copyright 2011 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

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
    let toCell = Cell . (== '1')
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
       
