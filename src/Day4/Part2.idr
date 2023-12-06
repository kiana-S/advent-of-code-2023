module Day4.Part2

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


incrementN : Nat -> Nat -> List (Nat, a) -> List (Nat, a)
incrementN _ _ [] = []
incrementN Z _ xs = xs
incrementN (S n) num ((n', x) :: xs) = (num + n', x) :: incrementN n num xs

compute : List Card -> Nat
compute = go . map (1,)
  where
    go : List (Nat, Card) -> Nat
    go [] = 0
    go input@((num, card) :: cards) =
      let winning = count (`elem` card.winning) card.received
      in  num + go (assert_smaller input $ incrementN winning num cards)


export
solution : String -> Maybe Nat
solution = map compute . traverse parseCard . lines
