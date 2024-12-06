module SolverTest where

import Circuit
import CircuitGraph (CircuitTopology (..), adjacencyMap, buildTopology)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Solver
import Test.HUnit

-- CIRCUIT GRAPH TESTING

-- Simple test circuit with 2 nodes and 2 components
simpleTestCircuit :: Circuit
simpleTestCircuit =
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
   in Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1)])

testCircuitTopology :: Test
testCircuitTopology = TestCase $ do
  -- Get the topology
  let topology = buildTopology simpleTestCircuit
      adjMap = adjacencyMap topology

  -- Verify n1 connects to n2 through both components
  let n1Connections = Map.findWithDefault [] (NodeID "n1") adjMap
  assertEqual "Number of connections from n1" 2 (length n1Connections)
  assertBool
    "n1 should connect to n2 through r1"
    ((NodeID "n2", ComponentID "r1") `elem` n1Connections)
  assertBool
    "n1 should connect to n2 through v1"
    ((NodeID "n2", ComponentID "v1") `elem` n1Connections)

  -- Verify n2 connects to n1 through both components
  let n2Connections = Map.findWithDefault [] (NodeID "n2") adjMap
  assertEqual "Number of connections from n2" 2 (length n2Connections)
  assertBool
    "n2 should connect to n1 through r1"
    ((NodeID "n1", ComponentID "r1") `elem` n2Connections)
  assertBool
    "n2 should connect to n1 through v1"
    ((NodeID "n1", ComponentID "v1") `elem` n2Connections)

-- EQUATION GENERATION TESTING

testKVLEquations :: Test
testKVLEquations = TestCase $ do
  -- Get KVL equations
  let topology = buildTopology simpleTestCircuit
      kvlEquations = getKVLEquations simpleTestCircuit topology

  -- Should have 1 equation for the single loop
  assertEqual "Number of KVL equations" 1 (length kvlEquations)

  -- The equation should be: 100*i_r1 + 5 = 0
  -- (resistor voltage drop + voltage source = 0 around loop)
  let equation = head kvlEquations
  case equation of
    Equation (Sum terms) (Constant 0.0) -> do
      assertEqual "Number of terms in KVL equation" 2 (length terms)
      -- Check the terms are as expected
      let expectedTerms =
            [ Product [Constant 100.0, UnknownTerm (Parameter (ComponentID "i_r1"))],
              Constant 5.0
            ]
      assertEqual "KVL equation terms" expectedTerms terms
    _ -> assertFailure "Unexpected equation format"

testKCLEquations :: Test
testKCLEquations = TestCase $ do
  -- Get KCL equations
  let topology = buildTopology simpleTestCircuit
      kclEquations = getKCLEquations simpleTestCircuit topology

  -- Should have 2 equations, one for each node
  assertEqual "Number of KCL equations" 2 (length kclEquations)

  -- For node n1: i_r1 - i_v1 = 0
  -- For node n2: -i_r1 + i_v1 = 0
  let expectedTerms1 =
        [ UnknownTerm (Parameter (ComponentID "i_r1")),
          Product [Constant (-1), UnknownTerm (Parameter (ComponentID "i_v1"))]
        ]
      expectedTerms2 =
        [ Product [Constant (-1), UnknownTerm (Parameter (ComponentID "i_r1"))],
          UnknownTerm (Parameter (ComponentID "i_v1"))
        ]

  -- Check first equation
  case head kclEquations of
    Equation (Sum terms) (Constant 0.0) -> do
      assertEqual "Number of terms in first KCL equation" 2 (length terms)
      assertEqual "First KCL equation terms" expectedTerms1 terms
    _ -> assertFailure "Unexpected first equation format"

  -- Check second equation
  case kclEquations !! 1 of
    Equation (Sum terms) (Constant 0.0) -> do
      assertEqual "Number of terms in second KCL equation" 2 (length terms)
      assertEqual "Second KCL equation terms" expectedTerms2 terms
    _ -> assertFailure "Unexpected second equation format"

runEquationTests :: IO Counts
runEquationTests = runTestTT $ TestList [testKVLEquations, testKCLEquations]

-- GENERAL TESTING

-- Simple circuit with 1 resistor and 1 voltage source in a loop
simpleCircuitTest :: Test
simpleCircuitTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1)])
  let solution = solve circuit
  assertEqual "Current through components" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)
  assertEqual "Voltage difference" 5.0 (fromJust (Map.lookup (NodeVoltage (NodeID "n1")) solution) - fromJust (Map.lookup (NodeVoltage (NodeID "n2")) solution))

-- Two resistors in series with voltage source
twoResistorsInSeriesTest :: Test
twoResistorsInSeriesTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n2") (NodeID "n3")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n3") (NodeID "n1")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "r2", r2), (ComponentID "v1", v1)])
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertEqual "Node n2 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

-- Two resistors in parallel with voltage source
twoResistorsInParallelTest :: Test
twoResistorsInParallelTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "r2", r2), (ComponentID "v1", v1)])
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertEqual "Total current" 0.2 (fromJust $ Map.lookup (Parameter (ComponentID "i_v1")) solution)
  assertEqual "Current through r1" 0.1 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

-- One resistor with two voltage sources
twoVoltageSourcesTest :: Test
twoVoltageSourcesTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n3") (NodeID "n1")
      v2 = Component (ComponentID "v2") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v2"))) (NodeID "n2") (NodeID "n3")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1), (ComponentID "v2", v2)])
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertEqual "Node n2 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

-- Two resistors and two voltage sources in a loop
twoVoltageSourcesInLoopTest :: Test
twoVoltageSourcesInLoopTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Unknown (NodeVoltage (NodeID "n3")))
      n4 = Node (NodeID "n4") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n3")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n3") (NodeID "n4")
      v2 = Component (ComponentID "v2") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v2"))) (NodeID "n4") (NodeID "n1")
      circuit =
        Circuit
          (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3), (NodeID "n4", n4)])
          (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1), (ComponentID "r2", r2), (ComponentID "v2", v2)])
  let solution = solve circuit
  assertEqual "Node n1 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertEqual "Node n2 voltage" 0.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertEqual "Node n3 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n3")) solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

tests :: Test
tests = TestList [simpleCircuitTest, twoResistorsInSeriesTest, twoResistorsInParallelTest, twoVoltageSourcesTest, twoVoltageSourcesInLoopTest]