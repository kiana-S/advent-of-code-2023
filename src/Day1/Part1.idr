module Day1.Part1

import Data.List
import Data.String

%default total


parseChar : Char -> Nat
parseChar = cast . String.singleton

parseLine : String -> Maybe Nat
parseLine str = case filter isDigit (unpack str) of
  digits@(_ :: _) =>
    let digit1 = parseChar (head digits)
        digit2 = parseChar (last digits)
    in Just $ digit1 * 10 + digit2
  [] => Nothing

export
solution : String -> Maybe Nat
solution = map sum . traverse parseLine . lines
