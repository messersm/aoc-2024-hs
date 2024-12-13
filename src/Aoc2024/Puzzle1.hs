module Aoc2024.Puzzle1 where

import Lib

import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MultiSet

import Control.Applicative ((<|>))
import Data.List (unzip)
import Data.Maybe
import Text.ParserCombinators.ReadP

-- | Parse the puzzle 1 input file
parser :: ReadP [(Int, Int)]
parser = many $ do
  x <- natural
  skipSpaces
  y <- natural
  skip newline <|> eof
  return (x, y)

part1 :: ReadP Int
part1 = do
  (xs, ys) <- unzip <$> parser
  let
    set1 = MultiSet.fromList xs
    set2 = MultiSet.fromList ys
    paired = zip (MultiSet.toAscList set1) (MultiSet.toAscList set2)
  return $ sum [abs (x - y) | (x, y) <- paired]

part2 :: ReadP Int
part2 = do
  (xs, ys) <- unzip <$> parser
  let
    rightMap = MultiSet.toMap $ MultiSet.fromList ys

  return $ sum [x * occur | x <- xs, let occur = Map.findWithDefault 0 x rightMap]
