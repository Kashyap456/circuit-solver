module CircuitGraphTest where

import Circuit
import CircuitGraph
  ( SearchState (..),
    adjacencyMap,
    buildTopology,
    findLoopPathsFromNode,
    makeLoopCircuit,
    nextMoves,
  )
import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Path
  ( Path,
    addPair,
    emptyPath,
    isLoop,
    pathComponents,
    pathNodes,
    startPath,
  )
import Test.HUnit

-- Simple test circuit with 2 nodes and 2 components forming a single loop
simpleLoopCircuit :: Circuit
simpleLoopCircuit =
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
   in Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1)])

simpleParallelCircuit :: Circuit
simpleParallelCircuit =
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n2")
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n1") (NodeID "n2")
   in Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "v1", v1), (ComponentID "r1", r1), (ComponentID "r2", r2)])

-- Test Path operations
testPathOperations :: Test
testPathOperations = TestCase $ do
  -- Test empty path
  let path1 = emptyPath
  assertBool "Empty path should not be a loop" $ not $ isLoop path1

  -- Test single node path
  let path2 = startPath (NodeID "n1")
  assertBool "Single node path should not be a loop" $ not $ isLoop path2

  -- Test building a loop path
  let maybePath3 = do
        p1 <- addPair (NodeID "n2") (ComponentID "r1") path2
        addPair (NodeID "n1") (ComponentID "v1") p1

  case maybePath3 of
    Nothing -> assertFailure "Failed to build valid path"
    Just path3 -> do
      assertBool "Should be a valid loop" $ isLoop path3
      assertEqual
        "Should have correct nodes"
        [NodeID "n1", NodeID "n2", NodeID "n1"]
        (pathNodes path3)
      assertEqual
        "Should have correct components"
        [ComponentID "r1", ComponentID "v1"]
        (pathComponents path3)

-- Test nextMoves with new Path-based state
testNextMoves :: Test
testNextMoves = TestCase $ do
  let topology = buildTopology simpleLoopCircuit
      initialState = SearchState (startPath (NodeID "n1")) Set.empty

  -- From n1 with no used components, should be able to move to n2 via either component
  -- Get all adjacent pairs from n1
  let adjacentPairs = fromMaybe [] (Map.lookup (NodeID "n1") (adjacencyMap topology))
  assertEqual "Number of adjacent pairs" 2 (length adjacentPairs)

  -- Should contain both r1 and v1 connections to n2
  let expectedPairs = [(NodeID "n2", ComponentID "r1"), (NodeID "n2", ComponentID "v1")]
  assertBool "Contains expected adjacent pairs" $ all (`elem` adjacentPairs) expectedPairs
  let moves = nextMoves topology initialState
  assertEqual "Number of possible moves" 2 (length moves)

  -- After using r1, should only be able to move via v1
  case moves of
    (firstMove : _) -> do
      let nextMovesList = nextMoves topology firstMove
      assertEqual "Moves after first component" 1 (length nextMovesList)
    [] -> assertFailure "Should have at least one move"

testFindLoops :: Test
testFindLoops = TestCase $ do
  let topology = buildTopology simpleLoopCircuit
      paths = findLoopPathsFromNode simpleLoopCircuit topology (NodeID "n1")

  assertEqual "Number of paths found" 1 (length paths)

  let firstPath = head paths
  assertEqual "Number of components in path" 2 (length $ pathComponents firstPath)
  assertEqual "Number of nodes in path" 3 (length $ pathNodes firstPath) -- Includes start node twice
  let pathComps = Set.fromList $ pathComponents firstPath
  assertBool "Path contains r1" $ Set.member (ComponentID "r1") pathComps
  assertBool "Path contains v1" $ Set.member (ComponentID "v1") pathComps

testFindLoopsParallel :: Test
testFindLoopsParallel = TestCase $ do
  let topology = buildTopology simpleParallelCircuit
      paths = findLoopPathsFromNode simpleParallelCircuit topology (NodeID "n1")
  assertEqual "Number of parallel paths found" 3 (length paths)

  forM_ paths $ \path -> do
    assertEqual "Number of components in path" 2 (length $ pathComponents path)
    assertEqual "Number of nodes in path" 3 (length $ pathNodes path) -- Includes start node twice
  let foundComponents = Set.fromList $ concatMap pathComponents paths
  assertEqual
    "Found all components"
    (Set.fromList [ComponentID "v1", ComponentID "r1", ComponentID "r2"])
    foundComponents

runAllGraphTests :: IO Counts
runAllGraphTests =
  runTestTT $
    TestList
      [ testPathOperations,
        testNextMoves,
        testFindLoops,
        testFindLoopsParallel
      ]
