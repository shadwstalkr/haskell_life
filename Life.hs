module Life
    ( Cell(..)
    , World()
    , renderWorld
    , stepWorld
    , newWorld
    , newWorld'
    ) where

import Data.List (foldl')
import qualified Data.Vector as V
import Graphics.Gloss

type Position = (Int, Int)

data Cell = Cell {position :: Position, is_live :: Bool}

type Population = V.Vector Cell

data World = World
             {
               width :: Int,
               height :: Int,
               population :: Population
             }

renderWorld :: World -> Picture
renderWorld = Pictures . V.toList . V.map renderCell . population

renderCell :: Cell -> Picture
renderCell (Cell (xx, yy) live)
    | live = Translate (fromIntegral xx) (fromIntegral yy) $ rectangleSolid 1 1
    | otherwise = Blank

getCell :: World -> Position -> Cell
getCell world (xx, yy) = (population world) V.! ((width world) * yy + xx)

neighborCells :: Population -> Int -> Position -> [Cell]
neighborCells pop width (px, py) = map (pop V.!) adjacentIndices
    where
      deltas = [-1, 0, 1]
      len = V.length pop
      inBounds idx = idx >= 0 && idx < len
      adjacentIndices = filter inBounds . map (\(dx, dy) -> ((py + dy) * width) + (px + dx)) $
                        [(x, y) | x <- deltas, y <- deltas, not (x == 0 && y == 0)]

countLiveCells :: [Cell] -> Int
countLiveCells = foldl' live 0
    where live acc (Cell _ cell_live)
              | cell_live = acc + 1
              | otherwise = acc

ageCell :: World -> Position -> Cell
ageCell world pos =
    let liveNeighbors = countLiveCells $ neighborCells (population world) (width world) pos
        cell_live = is_live (getCell world pos)
        lonely = liveNeighbors < 2
        crowded = liveNeighbors > 3
        spawn = liveNeighbors == 3 && not cell_live
    in Cell pos (spawn || (not lonely && not crowded && cell_live))

stepWorld :: World -> World
stepWorld world = world {population = V.map (ageCell world . position) $ population world}

newWorld :: Int -> Int -> World
newWorld ww hh = World ww hh $ V.fromList [Cell (xx, yy) False | xx <- xrange, yy <- yrange]
    where xrange = [0..(ww - 1)]
          yrange = [0..(hh - 1)]

newWorld' :: Int -> [Cell] -> World
newWorld' ww cells = World ww hh (V.fromList cells)
    where hh = floor $ fromIntegral (length cells) / fromIntegral ww
