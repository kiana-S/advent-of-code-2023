module Day4.Part1

import Data.Nat
import Data.List
import Data.List1
import Data.String

%default total

record Card where
  constructor MkCard
  winning, received : List Nat

windows : Nat -> String -> List String
windows _ "" = []
windows size str = substr 0 size str ::
  windows size (assert_smaller str $ substr size (length str `minus` size) str)

parseCard : String -> Maybe Card
parseCard str =
  let (_ ::: [numbers]) = split (==':') str
        | _ => Nothing
      (left ::: [right]) = split (=='|') numbers
        | _ => Nothing
      process = traverse parsePositive . filter (not . null) . map ltrim . windows 3
  in  MkCard <$> process left <*> process right

numWinning : Card -> Nat
numWinning (MkCard w r) = count (`elem` w) r

points : Nat -> Nat
points 0 = 0
points (S n) = power 2 n

export
solution : String -> Maybe Nat
solution = map (concatMap @{Additive} (points . numWinning))
  . traverse parseCard . lines
