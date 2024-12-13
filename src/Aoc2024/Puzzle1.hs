module Aoc2024.Puzzle1 where

import Lib

import qualified Data.Map.Strict as Map
import Data.List (unzip)
import Data.Maybe

import Control.Applicative ((<|>))

import Text.ParserCombinators.ReadP

import qualified Data.MultiSet as MultiSet

-- | Parse the puzzle 1 input file
parser :: ReadP [(Int, Int)]
parser = many $ do
  x <- natural
  skipSpaces
  y <- natural
  _ <- newline <|> eof
  return (x, y)

part1 :: String -> Maybe Int
part1 content = do
  (xs, ys) <- unzip <$> runParser parser content
  let
    set1 = MultiSet.fromList xs
    set2 = MultiSet.fromList ys
    paired = zip (MultiSet.toAscList set1) (MultiSet.toAscList set2)
  return $ sum [abs (x - y) | (x, y) <- paired]

part2 :: String -> Maybe Int
part2 content = do
  (xs, ys) <- unzip <$> runParser parser content
  let
    rightMap = MultiSet.toMap $ MultiSet.fromList ys

  return $ sum [x * occur | x <- xs, let occur = Map.findWithDefault 0 x rightMap]
