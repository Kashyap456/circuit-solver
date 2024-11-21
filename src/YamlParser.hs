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

parseIndentation :: IParser String
parseIndentation = do
  sp <- many (satisfy isSpace)
  putState (length sp)
  return sp

parseString :: IParser YAMLValue
parseString = undefined

parseDouble :: IParser YAMLValue
parseDouble = undefined

parseList :: IParser YAMLValue
parseList = undefined

parseKeyValue :: IParser (String, YAMLValue)
parseKeyValue = undefined

parseMap :: IParser YAMLValue
parseMap = undefined

-- checkFileContents :: String -> IO (Either IO String IO String)
{-
parseYAMLFile :: String -> Either (ParseError, YAMLValue)
parseYAMLFile fn = runParserT parseMap 0 fn
-}

parseYAMLFile :: String -> Identity (Either ParseError YAMLValue)
parseYAMLFile = runParserT parseMap 0 "stdout"
