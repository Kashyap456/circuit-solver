module YamlParser where

import Circuit (Circuit, Component, Node)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Char
import Data.Map (Map)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Text.Parsec (ParsecT, many1, modifyState, noneOf, putState, runParserT)
import Text.Parsec.String
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (char)
import Control.Monad (guard)
import Data.Text (stripEnd, pack, unpack)
import Data.Text.Read (double)
import Text.Read

-- Keep track of the number of indents that are made
type Indentation = Int

-- Data types for parsing
data YAMLValue
  = YAMLString String
  | YAMLDouble Double
  | YAMLList [YAMLValue]
  | YAMLMap (Map String YAMLValue)
  deriving (Show, Eq)

type IParser = ParsecT String Indentation Identity

-- Parses white space.
parseWS :: IParser String
parseWS = many (satisfy isSpace)

-- Parses the indention of a string and updates the state.
parseIndentation :: IParser String
parseIndentation = do
  sp <- parseWS
  putState (length sp)
  return sp

-- Parses the indentation of a string and compares it against the state.
-- Used to check if this line is a continution of the previous (only succeed if
-- the indentation is greater)
checkIndentation :: IParser String
checkIndentation = do
  sp <- parseWS
  st <- getState
  guard (length sp > st)
  return sp

-- Parse text until the newline character.
parseLine :: IParser String
parseLine = do
  sp <- parseWS
  text <- many (satisfy (/= '\n'))
  nl <- char '\n'
  return (unpack (stripEnd (pack text)))

tryParseLine :: IParser String
tryParseLine = do
  checkIndentation
  parseLine

-- Parse as long as indentation does not match the initial indentation
parseLines :: IParser String
parseLines = do
  sp <- parseIndentation
  fst <- parseLine
  rst <- many tryParseLine
  return (unwords (fst : rst))

parseString :: IParser YAMLValue
parseString = YAMLString <$> parseLines

parseDouble :: IParser YAMLValue
parseDouble = do
  line <- parseLines
  case readMaybe line :: Maybe Double of
    Just d -> return (YAMLDouble d)
    Nothing -> fail "Not double"

parseList :: IParser YAMLValue
parseList = undefined

parseKeyValue :: IParser (String, YAMLValue)
parseKeyValue = undefined

parseMap :: IParser YAMLValue
parseMap = undefined

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
