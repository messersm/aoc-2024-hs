module Aoc2024.Puzzle5 where

import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

import Lib

newtype Rule = Rule (Int, Int) deriving (Show)
newtype Update = Update [Int] deriving (Show)

complies :: Update -> Rule -> Bool
complies (Update ps) (Rule (p1, p2)) = fromMaybe True $ pure (<) <*> i1 <*> i2
  where
    i1 = elemIndex p1 ps
    i2 = elemIndex p2 ps

middlePage :: Update -> Int
middlePage (Update ps) = ps !! (length ps `div` 2)

rule :: ReadP Rule
rule = do
  x <- natural
  skip $ char '|'
  y <- natural

  return $ Rule (x, y)

update :: ReadP Update
update = Update <$> sepBy1 natural (char ',')

parser :: ReadP ([Rule], [Update])
parser = do
  rules <- sepBy1 rule newline
  newline
  newline
  updates <- sepBy1 update newline
  skipMany newline
  eof

  return (rules, updates)


part1 :: ReadP Int
part1 = do
  (rules, updates) <- parser
  let correct = [u | u <- updates, all (complies u) rules]
  return $ sum $ middlePage <$> correct


part2 :: ReadP Int
part2 = undefined
  