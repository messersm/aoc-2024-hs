module Aoc2024.Puzzle3 where

import Control.Applicative ((<|>))
import Data.List (singleton)
import Text.ParserCombinators.ReadP

import Lib

data Instruction = Do | Dont | Mul Int Int deriving (Show)

doit :: ReadP Instruction
doit = string "do()" *> return Do

dont :: ReadP Instruction
dont = string "don't()" *> return Dont

mul :: ReadP Instruction
mul = do
  skip $ string "mul("
  x <- natural
  skip $ string ","
  y <- natural
  skip $ string ")"
  return $ Mul x y

parser :: ReadP [Instruction]
parser = do
    instructions <- many $ instr <++ noop
    eof
    return $ concat instructions
  where
    instr :: ReadP [Instruction]
    instr = singleton <$> choice [doit, dont, mul]

    noop :: ReadP [Instruction]
    noop = get *> return []

multiply :: Instruction -> Int
multiply Do        = 0
multiply Dont      = 0
multiply (Mul x y) = x * y

part1 :: ReadP Int
part1 = sum . map multiply <$> parser

-- | Reduce a list of instructions by applying all do's and don't
reduce :: Bool -> [Instruction] -> [Instruction]
reduce _  []        = []
reduce _  (Do:xs)   = reduce True xs
reduce _  (Dont:xs) = reduce False xs
reduce True  (m:xs) = m : reduce True xs
reduce False (_:xs) = reduce False xs

part2 :: ReadP Int
part2 = sum . map multiply . reduce True <$> parser
