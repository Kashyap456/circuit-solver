module YamlParserTest where

import Data.Map qualified as Map
import Test.HUnit
import YamlParser

compareYAML :: String -> YAMLValue -> IO ()
compareYAML filename expected = do
  result <- parseYAMLFile filename
  case result of
    (Left _) -> assert False
    (Right actual) -> assert (actual == expected)

parseSimpleTest :: Test
parseSimpleTest =
  let n1 = YAMLMap (Map.fromList [("id", YAMLString "n1"), ("voltage", YAMLString "n1")])
      n2 = YAMLMap (Map.fromList [("id", YAMLString "n2"), ("voltage", YAMLString "n1")])
      r1 =
        YAMLMap
          ( Map.fromList
              [ ("id", YAMLString "r1"),
                ("current", YAMLString "i_r1"),
                ("pos", YAMLString "n1"),
                ("neg", YAMLString "n2"),
                ("type", YAMLString "resistor"),
                ("resistance", YAMLDouble 100.0)
              ]
          )
      v1 =
        YAMLMap
          ( Map.fromList
              [ ("id", YAMLString "v1"),
                ("current", YAMLString "i_r1"),
                ("pos", YAMLString "n1"),
                ("neg", YAMLString "n2"),
                ("type", YAMLString "voltage"),
                ("voltage", YAMLDouble 5.0)
              ]
          )
      expected =
        YAMLMap
          ( Map.fromList
              [ ("nodes", YAMLList [n1, n2]),
                ("components", YAMLList [r1, v1])
              ]
          )
   in "Parse Simple" ~: compareYAML "test/sample_files/simple.yaml" expected

parseEmptyTest :: Test
parseEmptyTest = "Parse Empty" ~: compareYAML "test/sample_files/empty.yaml" YAMLNull
