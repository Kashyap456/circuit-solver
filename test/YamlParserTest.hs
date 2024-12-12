module YamlParserTest where
import qualified Data.Map as Map
import Test.HUnit
import Test.QuickCheck (Gen, Arbitrary (arbitrary), elements, listOf)
import Test.QuickCheck qualified as QC
import Data.Map qualified as Map
import YamlParser ( YAMLValue(..), parseYAMLFile, parseYAML )
import Data.Map (Map)
import qualified Data.List as List
import Text.Parsec (runParser)

--- UNIT TESTCASES ---
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

--- YAMLValue to String ---

isSpecial :: YAMLValue -> Bool
isSpecial yaml = case yaml of
  YAMLList _ -> True
  YAMLMap _ -> True
  _ -> False

getIndentation :: Int -> String
getIndentation n = replicate n ' '

convertYAMLValueToString :: YAMLValue -> Int -> String
convertYAMLValueToString yaml indentation =
  let spaces = getIndentation indentation in
    case yaml of
      YAMLString s -> spaces ++ s ++ "\n"
      YAMLDouble d -> spaces ++ show d ++ "\n"
      YAMLList l -> convertYAMLListToString l indentation
      YAMLMap m -> convertYAMLMapToString m indentation
      _ -> ""

convertKeyValueToString :: String -> YAMLValue -> Int -> String
convertKeyValueToString k v indent =
  let special = isSpecial v in
    let keyStr = getIndentation indent ++ k ++ ":" ++ (if special then "\n" else " ") in
      let valueStr = convertYAMLValueToString v (if special then indent + 1 else 0) in
        keyStr ++ valueStr

convertYAMLMapToString :: Map String YAMLValue -> Int -> String
convertYAMLMapToString m indent = Map.foldrWithKey f "" m where
  f k v acc = convertKeyValueToString k v indent ++ acc

convertYAMLListToString :: [YAMLValue] -> Int -> String
convertYAMLListToString l indent = List.foldr f "" l where
  f v acc =
    let vStr = case v of
          YAMLMap m -> convertYAMLMapToString m (indent + 1)
          YAMLList l' -> convertYAMLListToString l' (indent + 1)
          _ -> convertYAMLValueToString v 0 in
      getIndentation indent ++ "-" ++ (if isSpecial v then "\n" else " ") ++ vStr ++ acc

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
    elts <- genSmallList arbitrary
    return (YAMLList elts)

genKeyValuePair :: Gen (String, YAMLValue)
genKeyValuePair = do
    key <- genSafeString
    value <- arbitrary
    return (key, value)

genYAMLMap :: Gen YAMLValue
genYAMLMap = do
    elts <- genSmallList genKeyValuePair
    return (YAMLMap (Map.fromList elts))

instance Arbitrary YAMLValue where
  arbitrary :: Gen YAMLValue
  arbitrary = QC.frequency [
        (4, genYAMLString),
        (4, genYAMLDouble),
        (2, genYAMLList),
        (2, genYAMLMap)
    ]
  shrink :: YAMLValue -> [YAMLValue]
  shrink v = case v of
    YAMLString s -> []
    YAMLDouble d -> map YAMLDouble (QC.shrink d)
    YAMLList l -> map (\x -> if null x then YAMLNull else YAMLList x) (QC.shrinkList QC.shrink l)
    YAMLMap m -> map (\x -> if null x then YAMLNull else YAMLMap (Map.fromList x)) (QC.shrinkList QC.shrink (Map.toList m))
    YAMLNull -> [YAMLNull]

prop_roundtrip :: YAMLValue -> Bool
prop_roundtrip v = runParser (parseYAML True) 0 "" (convertYAMLValueToString v 0) == Right v