module Day6.Part1

import Data.String
import Data.List1

%default total


solveQuadratic : (a, b, c : Double) -> (Double, Double)
solveQuadratic a b c =
  let discrimSqrt = sqrt (b * b - 4.0 * a * c)
      rest = \x => (-b + x) / (2.0 * a)
      (s1, s2) = (rest (-discrimSqrt), rest discrimSqrt)
  in  (min s1 s2, max s1 s2)


parseInput : String -> List (Nat, Nat)
parseInput input =
  let process : String -> List Nat
      process str = case break isDigit str of
        (_, nums) => map cast $ filter (not . null) $ forget $ split (==' ') nums
      [time, dist] := process <$> lines input
        | _ => []
  in  zip time dist

numValid : (time, dist : Nat) -> Nat
numValid time dist =
  let (s1, s2) = solveQuadratic (-1.0) (cast time) (- cast dist)
  in  S $ cast s2 `minus` cast (ceiling s1)

export
solution : String -> Nat
solution = concatMap @{Multiplicative} (uncurry numValid) . parseInput
