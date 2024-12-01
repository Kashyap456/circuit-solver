module CircuitGraphTest where

import Circuit
import CircuitGraph
  ( SearchState (..),
    buildTopology,
    findLoopsFromNode,
    isValidNextNode,
    makeLoopCircuit,
    nextMoves,
    pathToCircuit,
  )
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.HUnit

-- Simple test circuit with 2 nodes and 2 components forming a single loop
simpleLoopCircuit :: Circuit
simpleLoopCircuit =
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
   in Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1)])

-- Test isValidNextNode helper
testIsValidNextNode :: Test
testIsValidNextNode = TestCase $ do
  -- Empty path - any node is valid
  assertBool "Empty path should allow any node" $
    isValidNextNode (NodeID "n1") []

  -- Single node path - any different node is valid
  assertBool "Single node path should allow different node" $
    isValidNextNode (NodeID "n2") [NodeID "n1"]

  -- Mid-path - can't revisit nodes except start
  assertBool "Mid-path should not allow revisiting non-start nodes" $
    not $
      isValidNextNode (NodeID "n2") [NodeID "n3", NodeID "n2", NodeID "n1"]

  -- Can complete loop back to start if path long enough
  assertBool "Should allow completing loop back to start" $
    isValidNextNode (NodeID "n3") [NodeID "n3", NodeID "n2", NodeID "n1"]

  -- Can't complete loop if path too short
  assertBool "Should not allow completing tiny loop" $
    not $
      isValidNextNode (NodeID "n1") [NodeID "n1"]

-- Test pathToCircuit helper
testPathToCircuit :: Test
testPathToCircuit = TestCase $ do
  -- Empty path should return Nothing
  assertEqual "Empty path" Nothing $
    pathToCircuit simpleLoopCircuit []

  -- Single node path should return Nothing
  assertEqual "Single node path" Nothing $
    pathToCircuit simpleLoopCircuit [NodeID "n1"]

  -- Valid loop should return Just Circuit
  let validPath = [NodeID "n1", NodeID "n2", NodeID "n1"]
  case pathToCircuit simpleLoopCircuit validPath of
    Nothing -> assertFailure "Valid loop path should return Just Circuit"
    Just circuit -> do
      assertEqual "Loop circuit nodes" 2 (Map.size $ nodes circuit)
      assertEqual "Loop circuit components" 2 (Map.size $ components circuit)

  -- Invalid path (not a loop) should return Nothing
  assertEqual "Non-loop path" Nothing $
    pathToCircuit simpleLoopCircuit [NodeID "n1", NodeID "n2"]

-- Test nextMoves helper
testNextMoves :: Test
testNextMoves = TestCase $ do
  let topology = buildTopology simpleLoopCircuit
      initialState =
        SearchState
          { currentNode = NodeID "n1",
            usedComponents = Set.empty,
            pathNodes = []
          }

  -- From n1 with no used components, should be able to move to n2 via either component
  let moves = nextMoves topology initialState
  assertEqual "Number of possible moves" 2 (length moves)

  -- After using r1, should only be able to move via v1
  let stateAfterR1 =
        SearchState
          { currentNode = NodeID "n2",
            usedComponents = Set.singleton (ComponentID "r1"),
            pathNodes = [NodeID "n1"]
          }
  let movesAfterR1 = nextMoves topology stateAfterR1
  assertEqual "Moves after using r1" 1 (length movesAfterR1)

  -- After using both components, should have no valid moves
  let stateAfterBoth =
        SearchState
          { currentNode = NodeID "n1",
            usedComponents = Set.fromList [ComponentID "r1", ComponentID "v1"],
            pathNodes = [NodeID "n2", NodeID "n1"]
          }
  let movesAfterBoth = nextMoves topology stateAfterBoth
  assertEqual "Moves after using both components" 0 (length movesAfterBoth)

-- Test makeLoopCircuit helper
testMakeLoopCircuit :: Test
testMakeLoopCircuit = TestCase $ do
  let path = [NodeID "n1", NodeID "n2", NodeID "n1"]
      loopCircuit = makeLoopCircuit simpleLoopCircuit path

  -- Should contain both nodes
  assertEqual "Loop nodes" 2 (Map.size $ nodes loopCircuit)
  assertBool "Contains n1" $ Map.member (NodeID "n1") (nodes loopCircuit)
  assertBool "Contains n2" $ Map.member (NodeID "n2") (nodes loopCircuit)

  -- Should contain both components
  assertEqual "Loop components" 2 (Map.size $ components loopCircuit)
  assertBool "Contains r1" $ Map.member (ComponentID "r1") (components loopCircuit)
  assertBool "Contains v1" $ Map.member (ComponentID "v1") (components loopCircuit)

-- Run all helper tests
runHelperTests :: IO Counts
runHelperTests =
  runTestTT $
    TestList
      [ testIsValidNextNode,
        testPathToCircuit,
        testNextMoves,
        testMakeLoopCircuit
      ]

testFindLoops :: Test
testFindLoops = TestCase $ do
  -- Get loops starting from n1
  let topology = buildTopology simpleLoopCircuit
      loops = findLoopsFromNode simpleLoopCircuit topology (NodeID "n1")

  -- Should find two loops
  -- n1 -> n2 -> n1
  -- n2 -> n1 -> n2
  -- TODO: add deduplication logic for loops
  assertEqual "Number of loops found" 2 (length loops)

  -- The loop should contain both components
  let loopCircuit = head loops
  assertEqual "Number of components in loop" 2 (Map.size $ components loopCircuit)
  assertEqual "Number of nodes in loop" 2 (Map.size $ nodes loopCircuit)

  -- Check that both components are present
  assertBool "Loop contains r1" (Map.member (ComponentID "r1") (components loopCircuit))
  assertBool "Loop contains v1" (Map.member (ComponentID "v1") (components loopCircuit))

runLoopTests :: IO Counts
runLoopTests = runTestTT testFindLoops

runAllGraphTests :: IO Counts
runAllGraphTests = runTestTT $ TestList [testFindLoops, testIsValidNextNode, testPathToCircuit, testNextMoves, testMakeLoopCircuit]
