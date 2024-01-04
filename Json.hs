module Json
--(JsonValue(..), parseJson)
where
import Control.Applicative
import Data.Char
import Data.Maybe
import Numeric

data JsonValue = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)


data JsonToken = TWhitespace
  | TNull
  | TTrue
  | TFalse
  | TNumber Double
  | TString String
  | TOpenObject
  | TCloseObject
  | TOpenArray
  | TCloseArray
  | TColon
  | TComma
  deriving (Eq, Show)

parseJson :: String -> Maybe JsonValue
parseJson jsonStr =
  parseTokens jsonStr >>=
    parseJsonElement >>=
      \(toks, v) -> if null toks then Just v else Nothing

type JsonParser = [JsonToken] -> Maybe ([JsonToken], JsonValue)

parseJsonElement :: JsonParser
parseJsonElement = parseJsonInWhitespace parseJsonValue

parseJsonInWhitespace :: JsonParser -> JsonParser
parseJsonInWhitespace jparser = parse where
  parse toks = jparser (parseJsonWhitespace toks) >>=
    \(toks', v) -> Just (parseJsonWhitespace toks', v)

parseJsonWhitespace :: [JsonToken] -> [JsonToken]
parseJsonWhitespace [] = []
parseJsonWhitespace (tok:toks)
  | tok == TWhitespace = parseJsonWhitespace toks
  | otherwise = tok:toks

parseJsonValue :: JsonParser
parseJsonValue [] = Nothing
parseJsonValue toks = parseBasicJsonValue toks
  <|> parseJsonValueString toks
  <|> parseJsonArray toks
  <|> parseJsonObject toks

parseBasicJsonValue :: JsonParser
parseBasicJsonValue (tok:toks) = case tok of
  TNull -> Just (toks, JsonNull)
  TTrue -> Just (toks, JsonBool True)
  TFalse -> Just (toks, JsonBool False)
  TNumber num -> Just (toks, JsonNumber num)
  _ -> Nothing

parseJsonValueString :: JsonParser
parseJsonValueString (tok:toks) = case tok of
  TString str -> Just (toks, JsonString str)
  _ -> Nothing

parseJsonArray :: JsonParser
parseJsonArray (tok:toks)
  | tok == TOpenArray = parseJsonArrayBody toks
  | otherwise = Nothing

parseJsonArrayBody :: JsonParser
parseJsonArrayBody toks =
  let (tok':toks') = parseJsonWhitespace toks in
    if tok' == TCloseArray then Just (toks', JsonArray [])
    else parseJsonArrayElements toks

parseJsonArrayElements :: JsonParser
parseJsonArrayElements toks =
  parseJsonElement toks >>=
    \(tok':toks', e) -> case tok' of
      TComma -> parseJsonArrayElements toks' >>= \(toks'', JsonArray es) -> Just (toks'', JsonArray (e:es))
      TCloseArray -> Just (toks', JsonArray [e])
      _ -> Nothing

parseJsonObject :: JsonParser
parseJsonObject (tok:toks)
  | tok == TOpenObject = parseJsonObjectBody toks
  | otherwise = Nothing

parseJsonObjectBody :: JsonParser
parseJsonObjectBody toks =
  let (tok':toks') = parseJsonWhitespace toks in
    if tok' == TCloseObject then Just (toks', JsonObject [])
    else parseJsonObjectMembers toks

parseJsonObjectMembers :: JsonParser
parseJsonObjectMembers toks =
  parseJsonObjectMember toks >>=
    \(tok':toks', kv) -> case tok' of
      TComma -> parseJsonObjectMembers toks' >>= \(toks'', JsonObject kvs) -> Just (toks'', JsonObject (kv:kvs))
      TCloseObject -> Just (toks', JsonObject [kv])
      _ -> Nothing

parseJsonObjectMember :: [JsonToken] -> Maybe ([JsonToken], (String, JsonValue))
parseJsonObjectMember toks =
  parseJsonInWhitespace parseJsonValueString toks >>=
    \(tok':toks', JsonString k) -> case tok' of
      TColon -> parseJsonElement toks' >>= \(toks'', v) -> Just (toks'', (k, v))
      _ -> Nothing

type TokenParser = String -> Maybe (String, JsonToken)

parseTokens :: String -> Maybe [JsonToken]
parseTokens [] = Just []
parseTokens input =
  parseToken input >>= \(input', tok) ->
    parseTokens input' >>= \toks -> Just (tok:toks)

parseToken :: TokenParser
parseToken input = parseTWhitespace input
  <|> parseIdentifierToken input
  <|> parseSymbolToken input
  <|> parseTNumber input 
  <|> parseTString input

parseTWhitespace :: TokenParser
parseTWhitespace [] = Nothing
parseTWhitespace (c:cs)
  | isSpace c = case parseTWhitespace cs of
      Nothing -> Just (cs, TWhitespace)
      Just v -> Just v
  | otherwise = Nothing

wordChars :: [Char]
wordChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

parseIdentifierStr :: String -> (String, String)
parseIdentifierStr input = parseIdenChar input []
  where
    parseIdenChar [] tok = ([], tok)
    parseIdenChar input tok =
      let c = head input in
        if c `elem` wordChars then parseIdenChar (tail input) (tok++[c])
        else (input, tok)

parseIdentifierToken :: TokenParser
parseIdentifierToken input = let (input', iden) = parseIdentifierStr input in
  case iden of
    "true" -> Just (input', TTrue)
    "false" -> Just (input', TFalse)
    "null" -> Just (input', TNull)
    _ -> Nothing

parseSymbolToken :: TokenParser
parseSymbolToken (c:cs) = case c of
  '{' -> Just (cs, TOpenObject)
  '}' -> Just (cs, TCloseObject)
  '[' -> Just (cs, TOpenArray)
  ']' -> Just (cs, TCloseArray)
  ':' -> Just (cs, TColon)
  ',' -> Just (cs, TComma)
  _ -> Nothing

parseNumberToken :: String -> String -> (String, String)
parseNumberToken [] tok = ([], tok)
parseNumberToken (c:cs) tok
  | isDigit c = parseNumberToken cs (tok ++ [c])
  | isSpace c || isJust (parseSymbolToken (c:cs)) = (c:cs, tok)
  | otherwise = ([], [])

isNumberTokenSeparator :: Char -> Bool
isNumberTokenSeparator c = isSpace c || isJust (parseSymbolToken [c])

parseTNumber :: TokenParser
parseTNumber input =
  let parsedNum = readFloat input in
    if null parsedNum then Nothing
    else let (num, input') = head parsedNum in
      if null input' || isNumberTokenSeparator (head input') then Just (input', TNumber num)
      else Nothing

parseStringLit :: String -> Maybe (String, String)
parseStringLit [] = Nothing
parseStringLit (c:cs)
  | c == '"' = Just (cs, [])
  | c == '\\' = parseEscapedStr cs
  | otherwise = parseStringLit cs >>= \(cs', strLit) -> Just (cs', c:strLit)

escapeCharMap :: [(Char, Char)]
escapeCharMap = [
  ('"', '"'),
  ('\\', '\\'),
  ('/', '/'),
  ('b', '\b'),
  ('f', '\f'),
  ('n', '\n'),
  ('r', '\r'),
  ('t', '\t')]

parseEscapedStr :: String -> Maybe (String, String)
parseEscapedStr [] = Nothing
parseEscapedStr (c:cs)
  | c == 'u' = parseHex cs >>= \(cs', hexVal) -> prependParseStrLit cs' (chr hexVal)
  | otherwise = lookup c escapeCharMap >>= prependParseStrLit cs
  where
    prependParseStrLit cs' c' = parseStringLit cs' >>= \(cs'', strLit) -> Just (cs'', c':strLit)

parseHex :: String -> Maybe (String, Int)
parseHex input = parseHexDigits input 4 0
  where
    parseHexDigits :: String -> Int -> Int -> Maybe (String, Int)
    parseHexDigits input 0 acc = Just (input, acc)
    parseHexDigits [] _ _ = Nothing
    parseHexDigits (digit:digits) remaining acc
      | isHexDigit digit = parseHexDigits digits (remaining - 1) (acc*16+digitToInt digit)
      | otherwise = Nothing

parseTString :: TokenParser
parseTString (c:cs)
  | c == '"' = parseStringLit cs >>= \(input', tok) -> Just (input', TString tok)
  | otherwise = Nothing
