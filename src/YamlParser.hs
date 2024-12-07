module YamlParser where

import Control.Monad.Except (Except)
import Control.Monad.Identity (Identity)
import Data.Char
import Data.Map (Map, fromList)
import Data.Text (pack, stripEnd, unpack)
import System.IO qualified as IO
import Text.Parsec (ParsecT, modifyState, putState, runParserT)
import Text.ParserCombinators.Parsec
import Text.Read

-- Keep track of the number of indents that are made
type Indentation = Int

-- Data types for parsing
data YAMLValue
  = YAMLString String
  | YAMLDouble Double
  | YAMLList [YAMLValue]
  | YAMLMap (Map String YAMLValue)
  | YAMLNull
  deriving (Show, Eq)

type IParser = ParsecT String Indentation Identity

type IParser' = ParsecT String Indentation (Except String)

--- HELPERS ---

-- Remove trailing whitespace.
trim :: String -> String
trim s = unpack $ stripEnd $ pack s

-- Check if a character is not a special character.
validChar :: Char -> Bool
validChar c = c /= '\n' && c /= '-' && c /= ':'

--- CHAR PARSERS ---

-- Parses a single space (that isn't \n)
parseSpace :: IParser Char
parseSpace = satisfy (\c -> c /= '\n' && isSpace c)

parseEOF :: IParser Char
parseEOF = do
  eof
  return '\n'

parseEnd :: IParser Char
parseEnd = try (char '\n') <|> try parseEOF

--- STRING PARSERS ---

-- Parses white space (that isn't \n).
parseWS :: IParser String
parseWS = many (try parseSpace)

-- Parse text until the newline character.
parseLine :: IParser String
parseLine = do
  sp <- parseWS
  text <- many1 (try (satisfy validChar))
  try parseEnd
  return (trim text)

-- Parses the indention of a string and updates the state.
parseIndentation :: IParser String
parseIndentation = do
  sp <- parseWS
  putState (length sp)
  return sp

-- Parses the indentation of a string and compares it against the state.
checkIndentation :: IParser String
checkIndentation = do
  st <- getState
  sp <- parseWS
  if length sp == st
    then return sp
    else fail "Mismatch Indentation"

--- YAMLValue Parsers ---

-- Parses till it reaches a new line or EOF.
parseEmpty :: IParser YAMLValue
parseEmpty = do
  parseWS
  many (try (parseWS *> parseEnd))
  return YAMLNull

-- Parses a string.
parseString :: IParser YAMLValue
parseString = YAMLString <$> parseLine

-- Parses a double. Throws error if read value cannot be converted.
parseDouble :: IParser YAMLValue
parseDouble = do
  line <- parseLine
  case readMaybe line :: Maybe Double of
    Just d -> return (YAMLDouble d)
    Nothing -> fail "Not double"

-- Parses a single item within a YAML list.
parseListItem :: IParser YAMLValue
parseListItem = do
  char '-'
  ws <- parseWS
  s <- getState
  modifyState (+ (1 + length ws))
  res <- try (parseYAML False)
  putState s
  return res

-- Parses a YAML list.
parseList :: Bool -> IParser YAMLValue
parseList setIndentation = do
  s <- getState
  if setIndentation then parseIndentation else parseWS
  fst <- parseListItem
  rst <- many (try checkIndentation *> parseListItem)
  putState s
  return (YAMLList (fst : rst))

-- Parses a single key-value pair within a YAML map.
parseKeyValue :: IParser (String, YAMLValue)
parseKeyValue = do
  key <- many1 (try (satisfy validChar))
  char ':'
  ws <- try parseWS
  value <- try (parseYAML True)
  return (trim key, value)

-- Parses a YAML map.
parseMap :: Bool -> IParser YAMLValue
parseMap setIndentation = do
  s <- getState
  if setIndentation then parseIndentation else parseWS
  fst <- parseKeyValue
  rst <- many (try checkIndentation *> parseKeyValue)
  putState s
  return (YAMLMap (fromList (fst : rst)))

-- Attempts to parse all possible YAML values.
parseYAML :: Bool -> IParser YAMLValue
parseYAML setIndentation = do
  many (try (parseWS <* parseEnd))
  try (parseList setIndentation)
    <|> try (parseMap setIndentation)
    <|> try parseDouble
    <|> try parseString
    <|> try parseEmpty

-- Parses the given YAML file.
parseYAMLFile :: FilePath -> IO (Either ParseError YAMLValue)
parseYAMLFile filename = do
  handle <- IO.openFile filename IO.ReadMode
  str <- IO.hGetContents handle
  if null str
    then return (Right YAMLNull)
    else pure $ runParser (parseYAML True <* eof) 0 "" str

--- Extractors ---

extractString :: YAMLValue -> Maybe String
extractString (YAMLString s) = Just s
extractString _ = Nothing

extractDouble :: YAMLValue -> Maybe Double
extractDouble (YAMLDouble d) = Just d
extractDouble _ = Nothing

extractList :: YAMLValue -> Maybe [YAMLValue]
extractList (YAMLList l) = Just l
extractList _ = Nothing

extractMap :: YAMLValue -> Maybe (Map String YAMLValue)
extractMap (YAMLMap m) = Just m
extractMap _ = Nothing
