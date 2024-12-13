module Aoc2024.Puzzle2 where

import Debug.Trace (trace)

import Control.Applicative ((<|>))
import Data.List
import Text.ParserCombinators.ReadP

import Lib

parser :: ReadP [[Int]]
parser = many $ do
  xs <- sepBy1 natural space
  skip newline <|> eof
  return xs

normalize :: [Int] -> [Int]
normalize differences
  | hasNegative = negate <$> differences
  | otherwise   = differences
  where
    hasNegative = any (< 0) differences

-- | Check, whether a level difference is save
safe :: Int -> Bool
safe d = d >= 1 && d <= 3

isSafe :: [Int] -> Bool
isSafe levels = all safe $ normalize differences
  where
    differences = [(x - y) | (x, y) <- zip levels (drop 1 levels)]

part1 :: ReadP Int
part1 = do
  reports <- parser
  return $ length $ filter isSafe reports

-- | List of lists of items, with one item ignored each.
-- 
-- Example:
-- >>> dampen [1, 2, 3, 4]
-- [[2,3,4],[1,3,4],[1,2,4],[1,2,3]]
dampen :: [a] -> [[a]]
dampen xs = [ys ++ (drop 1 zs) | i <- [0..length xs - 1], let (ys, zs) = splitAt i xs]

part2 :: ReadP Int
part2 = do
  reports <- parser
  return $ length $ filter (\r -> any isSafe $ dampen r) reports
