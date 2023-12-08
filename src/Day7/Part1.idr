module Day7.Part1

import Data.SortedMap
import Data.String
import Data.List
import Data.List1
import Data.Vect
import Data.Singleton

%default total


-- TYPES

data Card = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace

valueCard : Card -> Nat
valueCard Two = 2
valueCard Three = 3
valueCard Four = 4
valueCard Five = 5
valueCard Six = 6
valueCard Seven = 7
valueCard Eight = 8
valueCard Nine = 9
valueCard Ten = 10
valueCard Jack = 11
valueCard Queen = 12
valueCard King = 13
valueCard Ace = 14

fromChar : Char -> Maybe Card
fromChar '2' = Just Two
fromChar '3' = Just Three
fromChar '4' = Just Four
fromChar '5' = Just Five
fromChar '6' = Just Six
fromChar '7' = Just Seven
fromChar '8' = Just Eight
fromChar '9' = Just Nine
fromChar 'T' = Just Ten
fromChar 'J' = Just Jack
fromChar 'Q' = Just Queen
fromChar 'K' = Just King
fromChar 'A' = Just Ace
fromChar _ = Nothing


Eq Card where
  (==) = (==) `on` valueCard

Ord Card where
  compare = compare `on` valueCard

data HandType = HighCard | OnePair | TwoPair | ThreeKind
              | FullHouse | FourKind | FiveKind

valueType : HandType -> Nat
valueType HighCard = 0
valueType OnePair = 1
valueType TwoPair = 2
valueType ThreeKind = 3
valueType FullHouse = 4
valueType FourKind = 5
valueType FiveKind = 6

Eq HandType where
  (==) = (==) `on` valueType

Ord HandType where
  compare = compare `on` valueType


-- HANDS

countCards : Vect n Card -> List (Card, Nat)
countCards = sortBy (compare `on` snd) . SortedMap.toList . go empty
  where
    go : forall n. SortedMap Card Nat -> Vect n Card -> SortedMap Card Nat
    go x [] = x
    go x (card :: cards) =
      go (update (Just . maybe 1 S) card x) cards

getHandType : Vect 5 Card -> HandType
getHandType hand = case countCards hand of
  [(_, 5)] => FiveKind
  [(_, 1), (_, 4)] => FourKind
  [(_, 2), (_, 3)] => FullHouse
  [(_, 1), (_, 1), (_, 3)] => ThreeKind
  [(_, 1), (_, 2), (_, 2)] => TwoPair
  [(_, 1), (_, 1), (_, 1), (_, 2)] => OnePair
  _ => HighCard

record Hand where
  constructor MkHand
  cards : Vect 5 Card
  -- store hand type directory for efficiency
  handType : Singleton (getHandType cards)

Eq Hand where
  MkHand c _ == MkHand c' _ = c == c'

Ord Hand where
  compare (MkHand c (Val h@(.(_)))) (MkHand c' (Val h'@(.(_)))) =
    compare h h' <+> compare c c'


-- MAIN CODE

zipStream : Stream a -> List b -> List (a, b)
zipStream _ [] = []
zipStream (x :: xs) (y :: ys) = (x, y) :: zipStream xs ys

parseLine : String -> Maybe (Hand, Nat)
parseLine line = do
  let (hand' ::: [bid']) = split (==' ') line
        | _ => Nothing
  hand <- toVect 5 =<< traverse fromChar (unpack hand')
  bid <- parsePositive bid'
  pure (MkHand hand (Val _), bid)

computeBids : List (Hand, Nat) -> Nat
computeBids =
  concatMap @{Additive} (\(i, _, bid) => i * bid)
  . zipStream [1..] . sortBy (compare `on` fst)


export
solution : String -> Maybe Nat
solution = map computeBids . traverse parseLine . lines
