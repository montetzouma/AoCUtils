module AoCUtils.Grid
  ( Coord
  , Direction(..)
  , coord2int
  , countColumns
  , countRows
  , findIndicesLists
  , findNeighbours
  , getElem
  , isInBounds )
where

import qualified Data.List as L


type Coord = (Int, Int)


data Direction
  = U
  | D
  | L
  | R 
  deriving (Eq, Ord, Show)


countColumns :: [[a]] -> Int
countColumns = length . head


countRows :: [[a]] -> Int
countRows = length


findIndicesLists :: (a -> Bool) -> [[a]] -> [Coord]
findIndicesLists p lists = indices
  where
    colsIndices  = map (L.findIndices p) lists
    rowsIndices  = L.findIndices (/= []) colsIndices
    colsIndices' = filter (/= []) colsIndices
    indices      = concat $ zipWith (\r c -> map (,r) c) rowsIndices colsIndices'


findNeighbours :: Bool -> Int -> Int -> Coord -> [Coord]
findNeighbours includeDiagonal nRows nCols (x,y) = filter (isInBounds nRows nCols) neighbours
  where
    nonDiagonalNeighbours = 
      [ (x  , y-1)
      , (x  , y+1)
      , (x+1, y  )
      , (x-1, y  ) ]

    diagonalNeighbours = 
      [ (x+1, y-1)
      , (x-1, y-1)
      , (x+1, y+1)
      , (x-1, y+1) ]
    
    neighbours = 
      if   includeDiagonal
      then nonDiagonalNeighbours <> diagonalNeighbours
      else nonDiagonalNeighbours


isInBounds :: Int -> Int -> Coord -> Bool
isInBounds nrows ncols (x,y) =  
     x >= 0
  && x <  ncols
  && y >= 0
  && y <  nrows


getElem :: [[a]] -> Coord -> a
getElem grid (x,y) = (grid !! y) !! x


coord2int :: Int -> Coord -> Int
coord2int nCols (x,y) = y * nCols + x