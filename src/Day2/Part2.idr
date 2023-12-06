module Day2.Part2

import Data.List
import Data.List1
import Data.String

import Day2.Part1

%default total


gamePower : Game -> Nat
gamePower (MkGame _ cubes) =
  let MkCubes r g b = concat cubes
  in  r * g * b

export
solution : String -> Maybe Nat
solution = map (sum . map gamePower) . traverse parseGame . lines
