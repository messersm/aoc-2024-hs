module Aoc2024.Puzzle4 where

import Data.Array.Unboxed
import Data.Array.IArray
-- import Data.Reader

import GHC.Natural
import Control.Monad
import Data.Ix

import Text.ParserCombinators.ReadP

import Lib

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Bounded, Enum, Show)

-- | The xy-delta associated with each direction.
delta :: Direction -> (Int, Int)
delta N  = ( 0,  1)
delta NE = ( 1,  1)
delta E  = ( 1,  0)
delta SE = ( 1, -1)
delta S  = ( 0, -1)
delta SW = (-1, -1)
delta W  = (-1,  0)
delta NW = (-1,  1)

-- get :: Int -> Int -> Int -> Direction -> (Reader (Array (Int, Int) String) String)
-- get x y length direction = undefined

parser :: ReadP (UArray (Int, Int) Char)
parser = do
  rows <- sepBy1 (many alpha) newline
  skipMany newline
  eof

  let bounds = case rows of
        []        -> ((0, 0), (0, 0))
        (first:_) -> ((1, 1), (length first, length rows))

  return $ listArray bounds $ concat rows

-- | From `(x, y)` walk at most the given number of steps in the given direction.
walk :: (Int, Int) -> Natural -> Direction -> (UArray (Int, Int) Char) -> [Char]
walk _ 0 _ _ = []
walk (x, y) len dir grid
    | inRange (bounds grid) (x, y) = grid ! (x, y) : walk (x', y') (len-1) dir grid
    | otherwise                    = []
  where
    (dx, dy) = delta dir
    -- y-axis is inversed:
    (x', y') = (x+dx, y-dy)
    
part1 :: ReadP Int
part1 = do
  grid <- parser

  let found = do
        (x, y) <- range $ bounds grid
        dir <- [minBound..maxBound] :: [Direction]
        let word = walk (x, y) 4 dir grid

        guard $ word == "XMAS"
        return ((x, y), dir, word)

  return $ length found

part2 :: ReadP Int
part2 = do
  grid <- parser

  let found = do
      -- We use (x, y) as the center of the X
        (x, y) <- range $ bounds grid
        let
          (dx1, dy1) = delta NW
          (x1, y1) = (x + dx1, y - dy1)
          word1 = walk (x1, y1) 3 SE grid

        guard $ word1 == "MAS" || word1 == "SAM"

        let
          (dx2, dy2) = delta SW
          (x2, y2) = (x + dx2, y - dy2)
          word2 = walk (x2, y2) 3 NE grid

        guard $ word2 == "MAS" || word2 == "SAM"

        return (x, y)

  return $ length found
