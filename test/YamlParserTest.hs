module YamlParserTest where

import YamlParser
import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import Data.Either (fromRight)
import Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (fromJust)

empty = YAMLMap Map.empty

parseSimple :: Test
parseSimple = TestCase $ do
    let n1 = YAMLMap (Map.fromList [("id", YAMLString "n1"), ("voltage", YAMLString "n1")])
        n2 = YAMLMap (Map.fromList [("id", YAMLString "n2"), ("voltage", YAMLString "n2")])
        r1 = YAMLMap (Map.fromList [
            ("id", YAMLString "r1"),
            ("current", YAMLString "i_r1"),
            ("pos", YAMLString "n1"),
            ("neg", YAMLString "n2"),
            ("type", YAMLString "resistor"),
            ("resistance", YAMLDouble 100.0)])
        v1 = YAMLMap (Map.fromList [
            ("id", YAMLString "v1"),
            ("current", YAMLString "i_r1"),
            ("pos", YAMLString "n2"),
            ("neg", YAMLString "n1"),
            ("type", YAMLString "voltage"),
            ("resistance", YAMLDouble 5.0)])
        expected = YAMLMap (Map.fromList [
            ("nodes", YAMLList [n1, n2]),
            ("components", YAMLList [r1, v1])])
    let actual = fromRight empty (runIdentity (parseYAMLFile "test/sample_files/simple.yaml"))
    assertEqual "Parse simple.yaml" expected actual

parseEmpty :: Test
parseEmpty = TestCase $ do
    let actual = fromRight empty (runIdentity (parseYAMLFile "test/sample_files/empty.yaml"))
    assertEqual "Parse empty.yaml" empty actual


{-
Possible QC Properties (For Later)

1) Saving a circuit and then parsing the saved file should result in the same
   circuit

2) Roundtrip properties
-}