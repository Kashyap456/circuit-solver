module SolverTest where

import Circuit
import CircuitGraph (CircuitTopology (..), adjacencyMap, buildTopology, findLoopPathsFromNode)
import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Numeric.LinearAlgebra qualified as LA
import Path (addPair, startPath)
import Solver
import Test.HUnit

-- HELPER FUNCTIONS

approxEqual :: Double -> Double -> Bool
approxEqual x y = abs (x - y) < 1e-10

assertApproxEqual :: String -> Double -> Double -> Assertion
assertApproxEqual msg expected actual =
  unless (approxEqual expected actual) $
    assertFailure $
      msg ++ "\nexpected: " ++ show expected ++ "\n but got: " ++ show actual

-- Helper functions for pretty printing (these should prob be moved to show instances)
printEquation :: Equation -> String
printEquation (Equation lhs rhs) =
  printTerm lhs ++ " = " ++ printTerm rhs

printTerm :: Term -> String
printTerm (Constant c) = show c
printTerm (UnknownTerm (NodeVoltage (NodeID n))) = "V_" ++ n
printTerm (UnknownTerm (Parameter (ComponentID p))) = p
printTerm (Sum terms) = "(" ++ intercalate " + " (map printTerm terms) ++ ")"
printTerm (Product terms) = intercalate "*" (map printTerm terms)

printMatrix :: LA.Matrix Double -> LA.Vector Double -> String
printMatrix m v =
  "Matrix A:\n" ++ show m ++ "\nVector b:\n" ++ show v

printSolution :: Map.Map Unknown Double -> String
printSolution sol =
  "Solution:\n"
    ++ concatMap
      ( \(k, v) -> case k of
          NodeVoltage (NodeID n) -> "V_" ++ n ++ " = " ++ show v ++ "V\n"
          Parameter (ComponentID p) -> p ++ " = " ++ show v ++ "A\n"
      )
      (Map.toList sol)

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

testLoopToKVLEquation :: Test
testLoopToKVLEquation = TestCase $ do
  -- path: n1 -> r2 -> n2 -> v1 -> n1
  let path = fromJust $ do
        p1 <- addPair (NodeID "n2") (ComponentID "r2") (startPath (NodeID "n1"))
        addPair (NodeID "n1") (ComponentID "v1") p1

  -- Get KVL equation for this loop in parallel circuit
  let equation = loopToKVLEquation simpleParallelCircuit path

  -- The equation should be: 100*i_r2 - 10 = 0
  -- (resistor voltage drop + voltage source = 0 around loop)
  case equation of
    Equation (Sum terms) (Constant 0.0) -> do
      assertEqual "Number of terms in KVL equation" 2 (length terms)
      -- Check the terms are as expected
      let expectedTerms =
            [ Product [Constant 100.0, UnknownTerm (Parameter (ComponentID "i_r2"))],
              Constant (-10.0)
            ]
      assertEqual "KVL equation terms" expectedTerms terms
    _ -> assertFailure "Unexpected equation format"

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

testComponentEquations :: Test
testComponentEquations = TestCase $ do
  -- Create a circuit with a voltage source and known node voltages
  let n1 = Node (NodeID "n1") (Known 5.0) -- Known voltage at n1
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2"))) -- Unknown voltage at n2
      v1 =
        Component
          (ComponentID "v1")
          (VSource (Unknown (Parameter (ComponentID "v_source")))) -- Unknown source voltage
          (Unknown (Parameter (ComponentID "i_v1")))
          (NodeID "n1")
          (NodeID "n2")
      circuit =
        Circuit
          (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)])
          (Map.fromList [(ComponentID "v1", v1)])

  let equations = getComponentEquations circuit
  assertEqual "Should generate one equation" 1 (length equations)

  case head equations of
    Equation lhs rhs -> do
      -- LHS should contain the unknown terms: v_n2 and -v_source
      case lhs of
        Sum terms -> do
          assertEqual "Should have two unknown terms" 2 (length terms)
          assertBool "Should contain node voltage term" $
            any (isUnknownNodeVoltage (NodeID "n2")) terms
          assertBool "Should contain source voltage term" $
            any (isUnknownSourceVoltage (ComponentID "v_source")) terms
        _ -> assertFailure "LHS should be a Sum"

      -- RHS should contain the constant term: -5.0 (from n1)
      case rhs of
        Sum [Product [Constant (-1), Constant v], _, _] ->
          assertApproxEqual "RHS constant should be -5.0" 5.0 v
        _ -> assertFailure "RHS should contain -5.0"
  where
    isUnknownNodeVoltage nid (UnknownTerm (NodeVoltage nid')) = nid == nid'
    isUnknownNodeVoltage _ _ = False

    isUnknownSourceVoltage cid (Product [Constant (-1), UnknownTerm (Parameter cid')]) =
      cid == cid'
    isUnknownSourceVoltage _ _ = False

runEquationTests :: IO Counts
runEquationTests = runTestTT $ TestList [testKVLEquations, testKCLEquations, testComponentEquations]

-- GENERAL TESTING

-- Simple circuit with 1 resistor and 1 voltage source in a loop
simpleCircuitTest :: Test
simpleCircuitTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n2")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1)])

  let solution = solve circuit
  assertApproxEqual "Current through components" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)
  assertApproxEqual "Voltage at n1" 5.0 (fromJust (Map.lookup (NodeVoltage (NodeID "n1")) solution))

-- Modified test with verbose output
twoResistorsInSeriesTest :: Test
twoResistorsInSeriesTest = TestCase $ do
  -- Create circuit
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n2") (NodeID "n3")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n3")
      circuit =
        Circuit
          (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)])
          (Map.fromList [(ComponentID "r1", r1), (ComponentID "r2", r2), (ComponentID "v1", v1)])

  -- Solve and print solution
  let solution = solve circuit

  -- Assertions
  assertApproxEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertApproxEqual "Node n2 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertApproxEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

-- Two resistors in parallel with voltage source
twoResistorsInParallelTest :: Test
twoResistorsInParallelTest = TestCase $ do
  -- Create circuit
  let circuit = simpleParallelCircuit
  let solution = solve circuit

  -- Assertions
  assertApproxEqual
    "Node n1 voltage should be 10.0V (same as voltage source)"
    10.0
    (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)

  assertApproxEqual
    "Total current through voltage source should be 0.2A (sum of branch currents)"
    0.2
    (fromJust $ Map.lookup (Parameter (ComponentID "i_v1")) solution)

  assertApproxEqual
    "Current through R1 should be 0.1A (V/R = 10V/100Ω)"
    0.1
    (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

  assertApproxEqual
    "Current through R2 should be 0.1A (V/R = 10V/100Ω)"
    0.1
    (fromJust $ Map.lookup (Parameter (ComponentID "i_r2")) solution)

-- One resistor with two voltage sources
twoVoltageSourcesTest :: Test
twoVoltageSourcesTest = TestCase $ do
  -- Create circuit
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n3")
      v2 = Component (ComponentID "v2") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v2"))) (NodeID "n3") (NodeID "n2")
      circuit =
        Circuit
          (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)])
          (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1), (ComponentID "v2", v2)])

  let solution = solve circuit

  -- Assertions
  assertApproxEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertApproxEqual "Node n2 voltage" (-5.0) (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertApproxEqual "Current through circuit" 0.15 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

-- Two resistors and two voltage sources in a loop
twoVoltageSourcesInLoopTest :: Test
twoVoltageSourcesInLoopTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      n3 = Node (NodeID "n3") (Unknown (NodeVoltage (NodeID "n3")))
      n4 = Node (NodeID "n4") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n4")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n3") (NodeID "n4")
      v2 = Component (ComponentID "v2") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v2"))) (NodeID "n3") (NodeID "n2")
      circuit =
        Circuit
          (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3), (NodeID "n4", n4)])
          (Map.fromList [(ComponentID "r1", r1), (ComponentID "v1", v1), (ComponentID "r2", r2), (ComponentID "v2", v2)])
  let solution = solve circuit
  assertApproxEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertApproxEqual "Node n2 voltage" 2.5 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertApproxEqual "Node n3 voltage" 7.5 (fromJust $ Map.lookup (NodeVoltage (NodeID "n3")) solution)
  assertApproxEqual "Current through circuit" 0.075 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

tests :: Test
tests = TestList [simpleCircuitTest, twoResistorsInSeriesTest, twoResistorsInParallelTest, twoVoltageSourcesTest, twoVoltageSourcesInLoopTest]

simpleParallelCircuit :: Circuit
simpleParallelCircuit =
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Known 0.0)
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      r2 = Component (ComponentID "r2") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r2"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 10.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n1") (NodeID "n2")
   in Circuit
        (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)])
        (Map.fromList [(ComponentID "r1", r1), (ComponentID "r2", r2), (ComponentID "v1", v1)])