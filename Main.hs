module Main where
import Json

main :: IO ()
main = do
  jsonStr <- readFile "example.json"
  print $ parseJson jsonStr
  return ()
