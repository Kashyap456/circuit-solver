module SolverTest where

import Circuit
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Solver
import Test.HUnit

-- Simple circuit with 1 resistor and 1 voltage source in a loop
simpleCircuitTest :: Test
simpleCircuitTest = TestCase $ do
  let n1 = Node (NodeID "n1") (Unknown (NodeVoltage (NodeID "n1")))
      n2 = Node (NodeID "n2") (Unknown (NodeVoltage (NodeID "n2")))
      r1 = Component (ComponentID "r1") (Resistor (Known 100.0)) (Unknown (Parameter (ComponentID "i_r1"))) (NodeID "n1") (NodeID "n2")
      v1 = Component (ComponentID "v1") (VSource (Known 5.0)) (Unknown (Parameter (ComponentID "i_v1"))) (NodeID "n2") (NodeID "n1")
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) [r1, v1]
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
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)]) [r1, r2, v1]
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
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2)]) [r1, r2, v1]
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
      circuit = Circuit (Map.fromList [(NodeID "n1", n1), (NodeID "n2", n2), (NodeID "n3", n3)]) [r1, v1, v2]
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
          [r1, v1, r2, v2]
  let solution = solve circuit
  assertEqual "Node n1 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n1")) solution)
  assertEqual "Node n2 voltage" 0.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n2")) solution)
  assertEqual "Node n3 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage (NodeID "n3")) solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter (ComponentID "i_r1")) solution)

tests :: Test
tests = TestList [simpleCircuitTest, twoResistorsInSeriesTest, twoResistorsInParallelTest, twoVoltageSourcesTest, twoVoltageSourcesInLoopTest]
