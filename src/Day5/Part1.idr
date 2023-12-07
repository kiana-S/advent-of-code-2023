module Day5.Part1

import Data.Nat
import Data.Maybe
import Data.List
import Data.List1
import Data.String
import Data.Morphisms

%default total


record MapEntry where
  constructor MkMapEntry
  dest, source, size : Nat

Map : Type
Map = List MapEntry

Almanac : Type
Almanac = List Map


parseInput : String -> Maybe (List1 Nat, Almanac)
parseInput input =
  let (first :: rest) = filter (not . null) $ lines input
        | [] => Nothing
      seeds = case break isDigit first of
        (_, nums) => traverse parsePositive $ split (==' ') nums
      sections = forget $ split (maybe False isAlpha . map fst . strUncons) rest
      maps = traverse @{Compose} (\line => do
        [dest, source, size] <- traverse parsePositive $ forget $ String.split (==' ') line
          | _ => Nothing
        pure $ MkMapEntry {dest,source,size}) sections
  in  (,) <$> seeds <*> maps

lookupMap : Map -> Nat -> Nat
lookupMap map n = fromMaybe n $ choiceMap lookupMapEntry map
  where
    lookupMapEntry : MapEntry -> Maybe Nat
    lookupMapEntry en = [ en.dest + diff | n >= en.source,
                                           let diff = n `minus` en.source,
                                           diff < en.size ]

lookupAlmanac : Almanac -> Nat -> Nat
lookupAlmanac = applyEndo . foldMap (Endo . lookupMap)


export
solution : String -> Maybe Nat
solution input = do
  (seeds, almanac) <- parseInput input
  pure $ foldr1 min $ map (lookupAlmanac almanac) seeds
