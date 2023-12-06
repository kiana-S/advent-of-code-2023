module Main

import Data.Nat
import Data.List1
import Data.String
import Language.Reflection

import AllDays

%language ElabReflection


partId : String
partId = "3-2"


fetchSolution : String -> Elab (String -> String)
fetchSolution idn = do
  let (day ::: [part]) = split (=='-') idn
      | _ => fail "Invalid identifier"
  let name = NS (MkNS ["Part" ++ part, "Day" ++ day]) `{solution}

  check `(\s => show (~(IVar EmptyFC name) s))
    <|> fail "\{show name} does not exist as a valid solution"

getUntilEmpty : IO String
getUntilEmpty = go ""
  where
    go : String -> IO String
    go str = do
      line <- getLine
      if line == ""
        then pure str
        else go (str ++ "\n" ++ line)

main : IO ()
main = do
  let sol = %runElab fetchSolution partId
  input <- trim <$> getUntilEmpty
  putStrLn $ sol input
