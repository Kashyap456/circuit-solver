module CircuitTest where

import Circuit
import CircuitModifiers
import CircuitSaver
import Control.Exception (catch, throwIO)
import Data.List qualified as List
import Data.Map qualified as Map
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Test.HUnit (Test (TestCase), assert, assertEqual, assertFailure, runTestTT)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    choose,
    chooseInt,
    elements,
    frequency,
    ioProperty,
    listOf1,
    oneof,
    quickCheck,
    vectorOf,
    (.&&.),
  )
import YamlParser

genNodeID :: Gen NodeID
genNodeID = do
  prefix <- elements ["n", "node"]
  num <- chooseInt (1, 20)
  return $ NodeID $ prefix ++ show num

genComponentID :: Gen ComponentID
genComponentID = do
  prefix <- elements ["v", "r", "vs", "res"]
  num <- chooseInt (1, 15)
  return $ ComponentID $ prefix ++ show num

genValue :: Gen Var
genValue =
  oneof
    [ Known <$> choose (-24, 24),
      Unknown . Parameter . ComponentID <$> do
        prefix <- elements ["p", "param"]
        num <- chooseInt (1, 10)
        return $ prefix ++ show num
    ]

genComponent :: Gen Component
genComponent = do
  cid <- genComponentID
  ctype <-
    frequency
      [ (1, VSource <$> genValue),
        (3, Resistor <$> genValue)
      ]
  curr <- genValue
  pos <- genNodeID
  Component cid ctype curr pos <$> genNodeID

genNode :: Gen Node
genNode = do
  nid <- genNodeID
  voltage <-
    oneof
      [ Known <$> choose (-12, 12),
        return $ Unknown (NodeVoltage nid)
      ]
  return $ Node nid voltage

-- Generator for Circuits
instance Arbitrary Circuit where
  arbitrary = do
    -- Create ground node first
    let groundNode = Node (NodeID "gnd") (Known 0.0)

    -- Generate additional nodes
    nodeCount <- choose (1, 4) -- One less since we have ground
    additionalNodes <- vectorOf nodeCount genNode
    let nodes = groundNode : additionalNodes

    -- Generate a small number of components
    compCount <- choose (1, 7)
    components <- vectorOf compCount $ do
      cid <- genComponentID
      ctype <-
        frequency
          [ (1, VSource . Known <$> choose (1, 12)),
            (3, Resistor . Known <$> choose (0.1, 1000))
          ]
      let curr = Unknown (Parameter cid)
      n1 <- elements nodes
      n2 <- elements (filter (/= n1) nodes)
      return $ Component cid ctype curr (nodeID n1) (nodeID n2)

    -- Create the circuit
    return $
      Circuit
        (Map.fromList [(nodeID n, n) | n <- nodes])
        (Map.fromList [(componentID c, c) | c <- components])

-- Properties

-- Every circuit should have at least one node and component
prop_nonEmpty :: Circuit -> Bool
prop_nonEmpty (Circuit ns cs) =
  not (Map.null ns) && not (null cs)

-- Node IDs in components should exist in nodes map
prop_nodeExists :: Circuit -> Bool
prop_nodeExists c@(Circuit ns cs) =
  case validate c of
    Nothing -> True
    Just _ ->
      all
        ( \comp ->
            Map.member (nodePos comp) ns
              && Map.member (nodeNeg comp) ns
        )
        cs

-- A validated circuit should maintain its structure
prop_validatePreservesStructure :: Circuit -> Property
prop_validatePreservesStructure c =
  case validate c of
    Nothing -> property True
    Just c' ->
      nodes c'
        `Map.isSubmapOf` nodes c
        .&&. components c'
        `Map.isSubmapOf` components c

-- Helper function to remove files from tests
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

-- Saving a circuit and parsing the saved file should give the same circuit
prop_savePreservesCircuit :: Circuit -> Property
prop_savePreservesCircuit c = ioProperty $ do
  saveCircuit "test.yaml" c
  parsed <- parseCircuit "test.yaml"
  removeIfExists "test.yaml"
  case parsed of
    Just c' -> return (c == c')
    Nothing -> return False

-- Test parsing a simple circuit with scientific notation values
testScientificNotation :: Test
testScientificNotation = TestCase $ do
  let n1 = Node (NodeID "n1") (Known 8.23456e-2)
  let n2 = Node (NodeID "n2") (Known 0.0)
  let r1 = Component (ComponentID "r1") (Resistor (Known 1.23456e-3)) (Unknown (Parameter (ComponentID "r1"))) (NodeID "n1") (NodeID "n2")
  let c = Circuit (Map.fromList [(nodeID n1, n1), (nodeID n2, n2)]) (Map.fromList [(componentID r1, r1)])
  saveCircuit "test.yaml" c
  parsed <- parseCircuit "test.yaml"
  -- removeIfExists "test.yaml"
  case parsed of
    Just c' -> assertEqual "Parsed circuit should match original" c c'
    Nothing -> assertFailure "Failed to parse circuit"
