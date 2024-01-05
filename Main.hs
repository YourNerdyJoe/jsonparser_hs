module Main where
import Json
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List
import Data.Maybe

showGameName :: JsonValue -> Maybe String
showGameName (JsonObject game) =
  case lookup "version" game of
    Just (JsonObject version) ->
      case lookup "name" version of
        Just (JsonString name) -> Just name
        _ -> Nothing
    _ -> Nothing
showGameName _ = Nothing

printPokemon :: String -> IO ()
printPokemon pkmn = do
  man <- newManager tlsManagerSettings
  request <- parseRequest $ "https://pokeapi.co/api/v2/pokemon/" ++ pkmn
  response <- httpLbs request man
  let obj = parseJson $ unpack $ responseBody response
  case fromJust obj of
    JsonObject kvs -> do
      case lookup "name" kvs of
        Just (JsonString name) -> putStrLn $ "Name: "++name
        _ -> putStrLn "No name???"
      case lookup "game_indices" kvs of
        Just (JsonArray games) -> do
          let gameNames = mapMaybe showGameName games
          putStrLn $ (++) "Games: " $ intercalate ", " gameNames
        _ -> putStrLn "No games???"
    _ -> putStrLn "No object"

main :: IO ()
main = do
  printPokemon "1"
  jsonStr <- readFile "example.json"
  let Just jsonObj = parseJson jsonStr
  print jsonObj
  let JsonObject obj = jsonObj
  let Just (JsonString escStr) = lookup "esc" obj
  putStrLn escStr
  let serStr = serializeJson jsonObj
  putStrLn serStr
  print $ parseJson serStr
  return ()
