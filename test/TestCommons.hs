module TestCommons where

import Circuit
import Control.Monad (unless)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra qualified as LA
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