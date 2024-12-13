module Main (main) where

import qualified Aoc2024.Puzzle1 as Puzzle1

import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.ReadP (ReadP)

import Lib (runParser)

puzzles :: [(ReadP String, ReadP String)]
puzzles =
  [ (show <$> Puzzle1.part1, show <$> Puzzle1.part2)
  ]

main :: IO ()
main = do
  args <- getArgs

  case args of
    [n, p, filename] -> do
      let puzzle = read n :: Int
      let part = read p :: Int

      if puzzle < 1 || puzzle > length puzzles
      then hPutStrLn stderr $ "Puzzle '" ++ n ++ "' does not exist or is not solved."
      else
        if part < 1 || part > 2
        then hPutStrLn stderr $ "Part must be 1 or 2."
        else do
          let parts = puzzles !! (puzzle - 1)
          let parser = if part == 1 then fst parts else snd parts

          content <- readFile filename
          putStrLn $ case runParser parser content of
              Just s  -> s
              Nothing -> "Failed to parse input."

    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <number> <part> <filename>"
      exitFailure
