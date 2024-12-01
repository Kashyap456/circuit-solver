module Solver where

import Circuit (Circuit (..), Component (..), ComponentID, ComponentType (..), Node (..), NodeID, Unknown, Var (..), componentType)
import CircuitGraph (CircuitTopology, buildTopology, findLoopsFromNode)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Numeric.LinearAlgebra qualified as LA

data Term
  = Constant Double
  | UnknownTerm Unknown
  | Sum [Term]
  | Product [Term]
  deriving (Show, Eq)

data Equation
  = Equation
  { lhs :: Term,
    rhs :: Term
  }

-- Find all loops in the circuit
findLoops :: Circuit -> [Circuit]
findLoops circuit =
  List.nub $
    concat
      [ findLoopsFromNode circuit topology nodeId
        | nodeId <- Map.keys (nodes circuit)
      ]
  where
    topology = buildTopology circuit

newtype Path = Path [NodeID]

newtype UsedComponents = UsedComponents (Set.Set ComponentID)

data SearchState = SearchState
  { currentNode :: NodeID,
    usedComponents :: UsedComponents,
    pathSoFar :: Path
  }

-- Simplify the current equation if possible
simplify :: Circuit -> Circuit
simplify = undefined

loopToKVLEquation :: Circuit -> Circuit -> Equation
loopToKVLEquation circuit loopCircuit =
  Equation
    { lhs = Sum [componentVoltage True comp | comp <- Map.elems (components loopCircuit)],
      rhs = Constant 0.0
    }

nodeToKCLEquation :: Circuit -> Node -> Equation
nodeToKCLEquation circuit node =
  Equation
    { lhs =
        Sum
          [ componentCurrent (nodePos comp == nodeID node) comp
            | comp <- Map.elems (components circuit),
              nodePos comp == nodeID node || nodeNeg comp == nodeID node
          ],
      rhs = Constant 0.0
    }

getKVLEquations :: Circuit -> CircuitTopology -> [Equation]
getKVLEquations circuit topology =
  let loops = findLoops circuit
   in map (loopToKVLEquation circuit) loops

getKCLEquations :: Circuit -> CircuitTopology -> [Equation]
getKCLEquations circuit topology =
  let equationNodes = Map.elems (nodes circuit)
   in map (nodeToKCLEquation circuit) equationNodes

-- Traverses the circuit and generates KVL and KCL equations
getEquations :: Circuit -> [Equation]
getEquations circuit =
  let topology = buildTopology circuit
   in getKVLEquations circuit topology ++ getKCLEquations circuit topology

equationsToMatrix :: [Equation] -> (LA.Matrix Double, LA.Vector Double)
equationsToMatrix = undefined

solve :: Circuit -> Map.Map Unknown Double
solve = undefined -- will use the getEquations helper above

componentVoltage :: Bool -> Component -> Term
componentVoltage isForward comp = case componentType comp of
  VSource v -> case v of
    Known val -> if isForward then Constant val else Constant (-val)
    Unknown u -> if isForward then UnknownTerm u else Product [Constant (-1), UnknownTerm u]
  Resistor r -> case (r, current comp) of
    (Known res, Unknown i) -> Product [Constant res, UnknownTerm i]
    (Unknown r', Known i) -> Product [UnknownTerm r', Constant i]
    (Unknown r', Unknown i) -> Product [UnknownTerm r', UnknownTerm i]
    (Known res, Known i) -> Constant (res * i)

componentCurrent :: Bool -> Component -> Term
componentCurrent isForward comp = case current comp of
  Known val ->
    if isForward
      then Constant val
      else Constant (-val)
  Unknown u ->
    if isForward
      then UnknownTerm u
      else Product [Constant (-1), UnknownTerm u]
