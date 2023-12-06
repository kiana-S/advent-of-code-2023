module Day2.Part1

import Data.List
import Data.List1
import Data.String

%default total


public export
record Cubes where
  constructor MkCubes
  red : Nat
  green : Nat
  blue : Nat

export
Semigroup Cubes where
  MkCubes r g b <+> MkCubes r' g' b' = MkCubes (max r r') (max g g') (max b b')

export
Monoid Cubes where
  neutral = MkCubes 0 0 0

public export
record Game where
  constructor MkGame
  num : Nat
  cubes : List Cubes


export
parseGame : String -> Maybe Game
parseGame str =
  case split (==':') str of
    (game ::: [cubes]) =>
      case break isDigit game of
        (_, num) => MkGame <$> (parsePositive num) <*> traverse (parseCubes . ltrim) (forget $ split (==';') cubes)
    _ => Nothing
  where
    parseCubes : String -> Maybe Cubes
    parseCubes str = map concat $ traverse (\s => case break (==' ') (ltrim s) of
      (x, " red") => Just $ MkCubes (cast x) 0 0
      (x, " green") => Just $ MkCubes 0 (cast x) 0
      (x, " blue") => Just $ MkCubes 0 0 (cast x)
      _ => Nothing) $ split (==',') str

validGame : Cubes -> Game -> Bool
validGame (MkCubes r g b) =
  all (\(MkCubes r' g' b') => r >= r' && g >= g' && b >= b') . cubes

export
solution : String -> Maybe Nat
solution = map (sum . map num . filter (validGame (MkCubes 12 13 14)))
  . traverse parseGame . lines
