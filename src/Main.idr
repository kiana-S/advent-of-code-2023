module Main

import Data.List1
import Data.String
import AllDays
import Language.Reflection

%language ElabReflection


partId : String
partId = "1-1"


fetchSolution : String -> Elab (String -> String)
fetchSolution str = do
  let (day ::: [part]) = split (=='-') str
      | _ => fail "Invalid identifier"
  let name = NS (MkNS ["Part" ++ part, "Day" ++ day]) `{solution}

  check `(\s => show (~(IVar EmptyFC name) s))
    <|> fail "Identifier does not point to valid solution"

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
