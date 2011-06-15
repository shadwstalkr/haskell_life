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

newtype Cell = Cell Bool

type Population = V.Vector Cell

data World = World
             {
               worldWidth :: Int,
               population :: Population
             }

-- Get a position from a population index
posFromIdx :: Int -> Int -> Position
posFromIdx width idx =
    let yy = floor (fromIntegral idx / fromIntegral width)
        xx = idx `mod` width
    in (xx, yy)

-- Get a population index from a position
idxFromPos :: Int -> Position -> Int
idxFromPos width (xx, yy) = width * yy + xx

-- Map a function over a world's population
mapWorld :: (Position -> Cell -> a) -> World -> V.Vector a
mapWorld fn (World width pop) = V.generate (V.length pop) mapCell
    where mapCell idx = fn (posFromIdx width idx) (pop V.! idx)

-- Render a world
renderWorld :: World -> Picture
renderWorld = Pictures . V.toList . mapWorld renderCell

-- Render a cell
renderCell :: Position -> Cell -> Picture
renderCell (xx, yy) (Cell live)
    | live = Translate (fromIntegral xx) (fromIntegral yy) $ rectangleSolid 1 1
    | otherwise = Blank

-- Get one cell from a world
getCell :: World -> Position -> Cell
getCell (World width pop) pos = pop V.! idxFromPos width pos

-- Get the cells around one cell
neighborCells :: World -> Position -> [Cell]
neighborCells (World width pop) (px, py) = map (pop V.!) adjacentIndices
    where
      deltas = [-1, 0, 1]
      len = V.length pop
      inBounds idx = idx >= 0 && idx < len
      adjacentIndices = filter inBounds . map (\(dx, dy) -> ((py + dy) * width) + (px + dx)) $
                        [(x, y) | x <- deltas, y <- deltas, not (x == 0 && y == 0)]

-- Count the number of live cells in a list
countLiveCells :: [Cell] -> Int
countLiveCells = foldl' live 0
    where live acc (Cell cell_live)
              | cell_live = acc + 1
              | otherwise = acc

-- Age a cell using Conway's life algorithm
ageCell :: World -> Position -> Cell -> Cell
ageCell world pos (Cell cell_live) =
    let liveNeighbors = countLiveCells $ neighborCells world pos
        lonely = liveNeighbors < 2
        crowded = liveNeighbors > 3
        spawn = liveNeighbors == 3 && not cell_live
    in Cell $ spawn || (not lonely && not crowded && cell_live)

-- Age all the cells in a world one step
stepWorld :: World -> World
stepWorld world = world {population = mapWorld (ageCell world) world}

-- Create a new, empty world
newWorld :: Int -> Int -> World
newWorld width height = World width $ V.replicate (width * height) (Cell False)

-- Create a new world from a population
newWorld' :: Int -> Population -> World
newWorld' width cells = World width cells
