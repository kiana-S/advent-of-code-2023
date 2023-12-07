module Day5.Part2

import Data.Nat
import Data.Maybe
import Data.List
import Data.List1
import Data.String
import Data.Morphisms

%default total

partitionMaybe : (a -> Maybe b) -> List a -> (List a, List b)
partitionMaybe f [] = ([], [])
partitionMaybe f (x :: xs) =
  let (lefts, rights) = partitionMaybe f xs
  in  case f x of
        Nothing => (x :: lefts, rights)
        Just x' => (lefts, x' :: rights)

record MapEntry where
  constructor MkEntry
  dest, source, size : Nat

(.end) : MapEntry -> Nat
entry.end = entry.source + pred entry.size

Map : Type
Map = List MapEntry

Almanac : Type
Almanac = List Map

Ranges : Type
Ranges = List (Nat, Nat)


toRanges : List Nat -> Maybe Ranges
toRanges [] = Just []
toRanges [_] = Nothing
toRanges (x :: y :: xs) = map ((x, x + pred y) ::) (toRanges xs)

parseInput : String -> Maybe (Ranges, Almanac)
parseInput input =
  let (first :: rest) = filter (not . null) $ lines input
        | [] => Nothing
      seeds = case break isDigit first of
        (_, nums) => toRanges =<< (traverse parsePositive $ forget $ split (==' ') nums)
      sections = forget $ split (maybe False isAlpha . map fst . strUncons) rest
      maps = traverse @{Compose} (\line => do
        [dest, source, size] <- traverse parsePositive $ forget $ String.split (==' ') line
          | _ => Nothing
        pure $ MkEntry {dest,source,size}) sections
  in  (,) <$> seeds <*> map @{Compose} (sortBy (compare `on` source)) maps


simplify : Ranges -> Ranges
simplify = go [] . filter (uncurry (<=))
  where
    union : (Nat, Nat) -> (Nat, Nat) -> Maybe (Nat, Nat)
    union (x,y) (x',y') =
      guard ((max x x' `minus` min y y') < 2) $> (min x x', max y y')

    go : Ranges -> Ranges -> Ranges
    go x [] = x
    go x (intv :: rs) =
      let (remaining, matches@(_ :: _)) = partitionMaybe (union intv) x
            | (_, []) => go (merge [intv] x) rs
          intvUnion = foldr1 (\(x,y),(x',y') => (min x x', max y y')) matches
      in  go (merge [intvUnion] remaining) rs

lookupMap : Map -> Ranges -> Ranges
lookupMap mp = simplify . (>>= lookupRange)
  where
    partition : (Nat, Nat) -> List (MapEntry, Nat, Nat)
    partition (x,y) = do
      en <- mp
      let start = max x en.source
          end = min y en.end
      guard (start <= end)
      pure (en, start, end)

    fillGaps : (Nat, Nat) -> List (MapEntry, Nat, Nat) -> List (Maybe MapEntry, Nat, Nat)
    fillGaps = go [<]
      where
        go : SnocList (Maybe MapEntry, Nat, Nat) ->
            (Nat, Nat) -> List (MapEntry, Nat, Nat) -> List (Maybe MapEntry, Nat, Nat)
        go out (x, y) [] = cast (out :< (Nothing, x, y))
        go out (x, y) ((en, x', y') :: xs) =
          if x == x'
          then go (out :< (Just en, x', y')) (S y', y) xs
          else go (out :< (Nothing, x, pred x') :< (Just en, x', y')) (S y', y) xs

    lookupRange : (Nat, Nat) -> Ranges
    lookupRange =
      map (uncurry $ maybe id $ \en => mapHom (\x => (x + en.dest) `minus` en.source))
      . uncurry fillGaps . mapSnd partition . dup

lookupAlmanac : Almanac -> Ranges -> Ranges
lookupAlmanac = applyEndo . foldMap (Endo . lookupMap)


export
solution : String -> Maybe Nat
solution input = do
  (seeds, almanac) <- parseInput input
  let loc@(_ :: _) = lookupAlmanac almanac seeds
        | [] => Nothing
  Just $ foldr1 min (fst <$> loc)
