module Day1.Part2

import Data.Fin
import Data.List
import Data.String

%default total


parseChar : Char -> Maybe Nat
parseChar = parsePositive . String.singleton

digitNames : List String
digitNames = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


parseLine' : List String -> String -> Maybe Nat
parseLine' digits str with (asList str)
  parseLine' digits "" | Nil = Nothing
  parseLine' digits str@(strCons _ _) | c :: cs =
    parseChar c <|>
      map (S . cast) (findIndex (`isPrefixOf` str) digits) <|>
      parseLine' digits _ | cs

parseFirst : String -> Maybe Nat
parseFirst = parseLine' digitNames

parseLast : String -> Maybe Nat
parseLast = parseLine' (reverse <$> digitNames) . reverse

parseLine : String -> Maybe Nat
parseLine str = Just $ !(parseFirst str) * 10 + !(parseLast str)


export
solution : String -> Maybe Nat
solution = map sum . traverse parseLine . lines
