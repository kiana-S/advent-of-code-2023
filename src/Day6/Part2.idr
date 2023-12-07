module Day6.Part2

import Data.String
import Data.List1

%default total


solveQuadratic : (a, b, c : Double) -> (Double, Double)
solveQuadratic a b c =
  let discrimSqrt = sqrt (b * b - 4.0 * a * c)
      rest = \x => (-b + x) / (2.0 * a)
      (s1, s2) = (rest (-discrimSqrt), rest discrimSqrt)
  in  (min s1 s2, max s1 s2)


parseInput : String -> (Nat, Nat)
parseInput input =
  let process : String -> Nat
      process str = case break isDigit str of
        (_, nums) => cast $ pack $ filter (/=' ') $ unpack nums
      [time, dist] := process <$> lines input
        | _ => (0, 0)
  in  (time, dist)

numValid : (time, dist : Nat) -> Nat
numValid time dist =
  let (s1, s2) = solveQuadratic (-1.0) (cast time) (- cast dist)
  in  S $ cast s2 `minus` cast (ceiling s1)

export
solution : String -> Nat
solution = uncurry numValid . parseInput
