module E2ETests where

import Circuit
  ( ComponentID (ComponentID),
    NodeID (NodeID),
    Unknown (Parameter)
  )
import CircuitSaver (parseCircuit)
import CircuitGraph (buildTopology, findLoopPathsFromNode)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Solver
import Test.HUnit
import TestCommons

simpleCircuitTest :: Test
simpleCircuitTest = TestCase $ do
  -- Parse the circuit from file
  maybeCircuit <- parseCircuit "test/sample_files/simple.yaml"
  let circuit = fromJust maybeCircuit

  -- Print the KVL paths for debugging
  putStrLn "KVL Paths:"
  let topology = buildTopology circuit
  mapM_ print (findLoopPathsFromNode circuit topology (NodeID "n1"))
  putStrLn ""
  -- Print the equations for debugging
  let eqns = getEquations circuit
  mapM_ (putStrLn . printEquation) eqns

  -- Print the matrix form
  let (matrix, vector) = equationsToMatrix eqns
  putStrLn $ printMatrix matrix vector

  -- Solve the circuit
  let solution = solve circuit

  -- Get the current through r1 (which should be 0.05A for a 5V source and 100Ω resistor)
  let i_r1 = case Map.lookup (Parameter (ComponentID "i_r1")) solution of
        Just current -> current
        Nothing -> error "Current not found in solution"

  -- Check the current matches expected value
  assertApproxEqual "Current should be approximately 0.05A" 0.05 i_r1

tests :: Test
tests =
  TestList
    [ "Simple Circuit E2E Test" ~: simpleCircuitTest
    ]
