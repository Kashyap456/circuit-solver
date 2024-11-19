module SolverTest where

import Circuit
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Solver
import Test.HUnit

-- Simple circuit with 1 resistor and 1 voltage source in a loop
simpleCircuitTest :: Test
simpleCircuitTest = TestCase $ do
  let n1 = Node "n1" (Unknown (NodeVoltage "n1"))
      n2 = Node "n2" (Unknown (NodeVoltage "n2"))
      r1 = Component "r1" (Resistor (Known 100.0)) (Unknown (Parameter "i_r1")) "n1" "n2"
      v1 = Component "v1" (VSource (Known 5.0)) (Unknown (Parameter "i_v1")) "n2" "n1"
      circuit = Circuit (Map.fromList [("n1", n1), ("n2", n2)]) [r1, v1]
  let solution = solve circuit
  assertEqual "Current through components" 0.05 (fromJust $ Map.lookup (Parameter "i_r1") solution)
  assertEqual "Voltage difference" 5.0 (fromJust (Map.lookup (NodeVoltage "n1") solution) - fromJust (Map.lookup (NodeVoltage "n2") solution))

-- Two resistors in series with voltage source
twoResistorsInSeriesTest :: Test
twoResistorsInSeriesTest = TestCase $ do
  let n1 = Node "n1" (Unknown (NodeVoltage "n1"))
      n2 = Node "n2" (Unknown (NodeVoltage "n2"))
      n3 = Node "n3" (Known 0.0)
      r1 = Component "r1" (Resistor (Known 100.0)) (Unknown (Parameter "i_r1")) "n1" "n2"
      r2 = Component "r2" (Resistor (Known 100.0)) (Unknown (Parameter "i_r2")) "n2" "n3"
      v1 = Component "v1" (VSource (Known 10.0)) (Unknown (Parameter "i_v1")) "n3" "n1"
      circuit = Circuit (Map.fromList [("n1", n1), ("n2", n2), ("n3", n3)]) [r1, r2, v1]
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage "n1") solution)
  assertEqual "Node n2 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage "n2") solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter "i_r1") solution)

-- Two resistors in parallel with voltage source
twoResistorsInParallelTest :: Test
twoResistorsInParallelTest = TestCase $ do
  let n1 = Node "n1" (Unknown (NodeVoltage "n1"))
      n2 = Node "n2" (Known 0.0)
      r1 = Component "r1" (Resistor (Known 100.0)) (Unknown (Parameter "i_r1")) "n1" "n2"
      r2 = Component "r2" (Resistor (Known 100.0)) (Unknown (Parameter "i_r2")) "n1" "n2"
      v1 = Component "v1" (VSource (Known 10.0)) (Unknown (Parameter "i_v1")) "n2" "n1"
      circuit = Circuit (Map.fromList [("n1", n1), ("n2", n2)]) [r1, r2, v1]
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage "n1") solution)
  assertEqual "Total current" 0.2 (fromJust $ Map.lookup (Parameter "i_v1") solution)
  assertEqual "Current through r1" 0.1 (fromJust $ Map.lookup (Parameter "i_r1") solution)

-- One resistor with two voltage sources
twoVoltageSourcesTest :: Test
twoVoltageSourcesTest = TestCase $ do
  let n1 = Node "n1" (Unknown (NodeVoltage "n1"))
      n2 = Node "n2" (Unknown (NodeVoltage "n2"))
      n3 = Node "n3" (Known 0.0)
      r1 = Component "r1" (Resistor (Known 100.0)) (Unknown (Parameter "i_r1")) "n1" "n2"
      v1 = Component "v1" (VSource (Known 10.0)) (Unknown (Parameter "i_v1")) "n3" "n1"
      v2 = Component "v2" (VSource (Known 5.0)) (Unknown (Parameter "i_v2")) "n2" "n3"
      circuit = Circuit (Map.fromList [("n1", n1), ("n2", n2), ("n3", n3)]) [r1, v1, v2]
  let solution = solve circuit
  assertEqual "Node n1 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage "n1") solution)
  assertEqual "Node n2 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage "n2") solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter "i_r1") solution)

-- Two resistors and two voltage sources in a loop
twoVoltageSourcesInLoopTest :: Test
twoVoltageSourcesInLoopTest = TestCase $ do
  let n1 = Node "n1" (Unknown (NodeVoltage "n1"))
      n2 = Node "n2" (Unknown (NodeVoltage "n2"))
      n3 = Node "n3" (Unknown (NodeVoltage "n3"))
      n4 = Node "n4" (Known 0.0)
      r1 = Component "r1" (Resistor (Known 100.0)) (Unknown (Parameter "i_r1")) "n1" "n2"
      v1 = Component "v1" (VSource (Known 10.0)) (Unknown (Parameter "i_v1")) "n2" "n3"
      r2 = Component "r2" (Resistor (Known 100.0)) (Unknown (Parameter "i_r2")) "n3" "n4"
      v2 = Component "v2" (VSource (Known 5.0)) (Unknown (Parameter "i_v2")) "n4" "n1"
      circuit =
        Circuit
          (Map.fromList [("n1", n1), ("n2", n2), ("n3", n3), ("n4", n4)])
          [r1, v1, r2, v2]
  let solution = solve circuit
  assertEqual "Node n1 voltage" 5.0 (fromJust $ Map.lookup (NodeVoltage "n1") solution)
  assertEqual "Node n2 voltage" 0.0 (fromJust $ Map.lookup (NodeVoltage "n2") solution)
  assertEqual "Node n3 voltage" 10.0 (fromJust $ Map.lookup (NodeVoltage "n3") solution)
  assertEqual "Current through circuit" 0.05 (fromJust $ Map.lookup (Parameter "i_r1") solution)

tests :: Test
tests = TestList [simpleCircuitTest, twoResistorsInSeriesTest, twoResistorsInParallelTest, twoVoltageSourcesTest, twoVoltageSourcesInLoopTest]
