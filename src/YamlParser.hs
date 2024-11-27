module YamlParser where

import Circuit (Circuit, Component, Node)
import Control.Monad (guard, when, unless)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Char
import Data.Map (Map, fromList)
import Data.Text (pack, stripEnd, unpack)
import Data.Text.Read (double)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Text.Parsec (ParsecT, many1, modifyState, noneOf, putState, runParserT)
import Text.Parsec.Char (char)
import Text.Parsec.String
import Text.ParserCombinators.Parsec
import Text.Read
import GHC.Base (IP)

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

-- Parses white space (that isn't \n).
parseWS :: IParser String
parseWS = many (satisfy (\c -> c /= '\n' && isSpace c))

-- Remove trailing whitespace.
trim :: String -> String
trim s = unpack $ stripEnd $ pack s

-- Parses the indention of a string and updates the state.
parseIndentation :: IParser String
parseIndentation = do
  sp <- parseWS
  putState (length sp)
  return sp

parseEmpty :: IParser YAMLValue
parseEmpty = do
  parseWS
  char '\n'
  return YAMLNull

-- Parses the indentation of a string and compares it against the state.
checkIndentation :: (Indentation -> Indentation -> Bool) -> IParser String
checkIndentation f = do
  sp <- parseWS
  st <- getState
  guard (f (length sp) st)
  return sp

-- Parse text until the newline character.
parseLine :: IParser String
parseLine = do
  sp <- parseWS
  text <- many1 (satisfy (/= '\n'))
  nl <- char '\n'
  return (trim text)

parseString :: IParser YAMLValue
parseString = YAMLString <$> parseLine

parseDouble :: IParser YAMLValue
parseDouble = do
  line <- parseLine
  case readMaybe line :: Maybe Double of
    Just d -> return (YAMLDouble d)
    Nothing -> fail "Not double"

parseListItem :: IParser YAMLValue
parseListItem = do
  char '-'
  ws <- parseWS
  s <- getState
  modifyState (+ (1 + length ws))
  res <- parseYAML
  putState s
  return res

parseList :: IParser YAMLValue
parseList = do
  parseIndentation
  fst <- parseListItem
  rst <- many (checkIndentation (==) *> parseListItem)
  return (YAMLList (fst : rst))

parseYAML :: IParser YAMLValue
parseYAML = do
  res <- try parseList <|> try (parseMap False) <|> try parseDouble <|> try parseString <|> parseEmpty
  if res /= YAMLNull then return res else parseYAML

parseKeyValue :: Bool -> IParser (String, YAMLValue)
parseKeyValue fst = do
  if fst then parseWS else checkIndentation (==)
  key <- many (satisfy (\c -> c /= ':' && c /= '\n'))
  char ':'
  value <- parseYAML
  return (trim key, value)

parseMap :: Bool -> IParser YAMLValue
parseMap setIndentation = do
  if setIndentation then parseIndentation else parseWS
  fst <- parseKeyValue True
  res <- many (parseKeyValue False)
  return (YAMLMap (fromList (fst : res)))


-- checkFileContents :: String -> IO (Either IO String IO String)
{-
parseYAMLFile :: String -> Identity (Either ParseError YAMLValue)
parseYAMLFile = runParserT parseMap 0 "stdout"

containsNewLine :: String -> Bool
containsNewLine = foldr (\x acc -> (x == '\n') || acc) False
-}

test :: FilePath -> IO (Either ParseError String)
test filename = do
  handle <- IO.openFile filename IO.ReadMode
  str <- IO.hGetContents handle
  pure $ runParser parseLine 0 "" str
