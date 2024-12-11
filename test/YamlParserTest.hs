module YamlParserTest where

import YamlParser (YAMLValue(..), parseYAMLFile)
import qualified Data.Map as Map
import Test.HUnit
import Test.QuickCheck (Gen, Arbitrary (arbitrary), elements, listOf)
import Test.QuickCheck qualified as QC

--- UNIT TESTCASES ---
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


--- GENERATORS ---
genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = do
    c <- genSafeChar
    s <- listOf genSafeChar
    return (c : s)

genYAMLString :: Gen YAMLValue
genYAMLString = YAMLString <$> genSafeString

genYAMLDouble :: Gen YAMLValue
genYAMLDouble = do
    numerator <- QC.chooseInt (0, 10000)
    denominator <- QC.chooseInt (1, 200)
    return (YAMLDouble (fromIntegral numerator / fromIntegral denominator))

genSmallList :: Gen a -> Gen [a]
genSmallList g = do
    fst <- g
    rst <- QC.resize 3 (listOf g)
    return (fst : rst)

genYAMLList :: Gen YAMLValue
genYAMLList = do
    elts <- genSmallList genYAMLValue
    return (YAMLList elts)

genKeyValuePair :: Gen (String, YAMLValue)
genKeyValuePair = do
    key <- genSafeString
    value <- genYAMLValue
    return (key, value)


genYAMLMap :: Gen YAMLValue
genYAMLMap = do
    elts <- genSmallList genKeyValuePair
    return (YAMLMap (Map.fromList elts))


genYAMLValue :: Gen YAMLValue
genYAMLValue =
    QC.frequency [
        (4, genYAMLString),
        (4, genYAMLDouble),
        (2, genYAMLList),
        (2, genYAMLMap)
    ]