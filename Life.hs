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
               worldHeight :: Int,
               population :: Population
             }

posFromIdx :: Int -> Int -> Position
posFromIdx width idx =
    let yy = floor (fromIntegral idx / fromIntegral width)
        xx = idx `mod` width
    in (xx, yy)

idxFromPos :: Int -> Position -> Int
idxFromPos width (xx, yy) = width * yy + xx

mapWorld :: (Position -> Cell -> a) -> World -> V.Vector a
mapWorld fn (World width height pop) = V.generate (V.length pop) mapCell
    where mapCell idx = fn (posFromIdx width idx) (pop V.! idx)

renderWorld :: World -> Picture
renderWorld = Pictures . V.toList . mapWorld renderCell

renderCell :: Position -> Cell -> Picture
renderCell (xx, yy) (Cell live)
    | live = Translate (fromIntegral xx) (fromIntegral yy) $ rectangleSolid 1 1
    | otherwise = Blank

getCell :: World -> Position -> Cell
getCell (World width _ pop) pos = pop V.! idxFromPos width pos

neighborCells :: World -> Position -> [Cell]
neighborCells (World width _ pop) (px, py) = map (pop V.!) adjacentIndices
    where
      deltas = [-1, 0, 1]
      len = V.length pop
      inBounds idx = idx >= 0 && idx < len
      adjacentIndices = filter inBounds . map (\(dx, dy) -> ((py + dy) * width) + (px + dx)) $
                        [(x, y) | x <- deltas, y <- deltas, not (x == 0 && y == 0)]

countLiveCells :: [Cell] -> Int
countLiveCells = foldl' live 0
    where live acc (Cell cell_live)
              | cell_live = acc + 1
              | otherwise = acc

ageCell :: World -> Position -> Cell -> Cell
ageCell world pos (Cell cell_live) =
    let liveNeighbors = countLiveCells $ neighborCells world pos
        lonely = liveNeighbors < 2
        crowded = liveNeighbors > 3
        spawn = liveNeighbors == 3 && not cell_live
    in Cell $ spawn || (not lonely && not crowded && cell_live)

stepWorld :: World -> World
stepWorld world = world {population = mapWorld (ageCell world) world}

newWorld :: Int -> Int -> World
newWorld width height = World width height $ V.replicate (width * height) (Cell False)
    where xrange = [0..(width - 1)]
          yrange = [0..(height - 1)]

newWorld' :: Int -> Population -> World
newWorld' width cells = World width height cells
    where height = floor $ fromIntegral (V.length cells) / fromIntegral width
