module Day3.Part2

import Data.Nat
import Data.List
import Data.List1
import Data.String
import Data.Maybe

%default total


record Number where
  constructor MkNumber
  line, start, len, value : Nat

adjacent : (Nat, Nat) -> Number -> Bool
adjacent (ln, col) (MkNumber ln' s l _) =
  (ln == ln' || S ln == ln' || ln == S ln') &&
  S col >= s && col <= (s + l)

parameters (schem : List String)

  nums : List Number
  nums = go 0 schem
    where
      parseLine : Nat -> Nat -> List Char -> List Number
      parseLine _ _ [] = []
      parseLine linum pos line@(c :: cs) =
          case span isDigit line of
            ([], _) => parseLine linum (S pos) cs
            (num, rest) => MkNumber linum pos (length num) (cast $ pack num)
              :: parseLine linum (pos + length num) (assert_smaller line $ drop (length num) line)

      go : Nat -> List String -> List Number
      go _ [] = []
      go n (line :: schem) = parseLine n 0 (unpack line) ++ go (S n) schem

  asterisks : List (Nat, Nat)
  asterisks = go 0 schem
    where
      parseLine : Nat -> List Char -> List Nat
      parseLine _ [] = []
      parseLine n ('*' :: cs) = n :: parseLine (S n) cs
      parseLine n (_ :: cs) = parseLine (S n) cs

      go : Nat -> List String -> List (Nat, Nat)
      go _ [] = []
      go n (line :: schem) = ((n,) <$> parseLine 0 (unpack line)) ++ go (S n) schem

  gearRatio : (Nat, Nat) -> Maybe Nat
  gearRatio ast = case filter (adjacent ast) nums of
    [x, y] => Just (x.value * y.value)
    _ => Nothing

  solution' : Nat
  solution' = sum $ catMaybes $ map gearRatio asterisks


export
solution : String -> Nat
solution = solution' . lines
