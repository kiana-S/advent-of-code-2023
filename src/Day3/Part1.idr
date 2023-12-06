module Day3.Part1

import Data.Nat
import Data.List
import Data.List1
import Data.String
import Data.Maybe

%default total


record Number where
  constructor MkNumber
  line, start, len, value : Nat


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
  
  surrounding : Number -> List Char
  surrounding (MkNumber ln s l _) = above ++ left ++ right ++ below
    where
      above : List Char
      above = if isZero ln
              then []
              else maybe [] unpack $
                substr (pred s) (if isZero s then S l else S (S l))
                <$> getAt (pred ln) schem

      below : List Char
      below = maybe [] unpack $
                substr (pred s) (if isZero s then S l else S (S l))
                <$> getAt (S ln) schem

      left : List Char
      left = case s of
              Z => []
              S s' => maybe [] unpack $ substr s' 1 <$> getAt ln schem

      right : List Char
      right = maybe [] unpack $ substr (s + l) 1 <$> getAt ln schem

  isPartNum : Number -> Bool
  isPartNum = any (\c => c /= '.' && not (isDigit c)) . surrounding

  solution' : Nat
  solution' = concatMap @{Additive} value $ filter isPartNum nums


export
solution : String -> Nat
solution = solution' . lines
