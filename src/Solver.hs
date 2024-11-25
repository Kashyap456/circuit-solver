module Solver where

import Circuit (Circuit, Unknown)
import Data.Map
import Numeric.LinearAlgebra qualified as LA

data Term
  = Constant Double
  | UnknownTerm Unknown
  | Sum [Term]
  | Product [Term]

data Equation
  = Equation
  { lhs :: Term,
    rhs :: Term
  }

-- Simplify the current equation if possible
simplify :: Circuit -> Circuit
simplify = undefined

-- Traverses the circuit and generates KVL and KCL equations
getEquations :: Circuit -> [Equation]
getEquations = undefined

equationsToMatrix :: [Equation] -> (LA.Matrix Double, LA.Vector Double)
equationsToMatrix = undefined

solve :: Circuit -> Map Unknown Double
solve = undefined -- will use the getEquations helper above