module Lib where

import Data.Char
import Text.ParserCombinators.ReadP

runParser :: ReadP a -> String -> Maybe a
runParser p content = case parsed of
    []    -> Nothing
    (x:_) -> Just $ fst x
  where
    parsed = filter (\(_, rest) -> rest == "") $ readP_to_S p content

digit :: ReadP Char
digit = satisfy isDigit

-- | Parse a natural number
natural :: ReadP Int
natural = read <$> many1 digit

skip :: ReadP a -> ReadP ()
skip p = p *> return ()

newline :: ReadP ()
newline = skip $ satisfy (== '\n')
