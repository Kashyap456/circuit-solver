module Solver where

import Circuit (Circuit (..), Component (..), ComponentID, ComponentType (..), Node (..), NodeID, Unknown, Var (..), componentType)
import CircuitGraph (CircuitTopology, buildTopology, findLoopsFromNode)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
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
  deriving (Show, Eq)

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
  let terms = [componentVoltage True comp | comp <- Map.elems (components loopCircuit)]
      -- Split terms into constants and variables
      (constants, variables) = List.partition isConstantTerm terms
      -- Sum up all constants and negate for RHS
      constantSum = case constants of
        [] -> Constant 0.0
        cs -> Product [Constant (-1.0), Sum cs]
   in Equation
        { lhs = Sum variables,
          rhs = constantSum
        }
  where
    isConstantTerm :: Term -> Bool
    isConstantTerm (Constant _) = True
    isConstantTerm (Product ts) = all isConstantTerm ts
    isConstantTerm _ = False

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
equationsToMatrix equations =
  let unknownIndices = getUnknownsWithIndices equations
      numUnknowns = Map.size unknownIndices
      numEquations = length equations
      -- Create matrix of coefficients and vector of constants
      matrix = LA.matrix numUnknowns $ concatMap (equationToRow unknownIndices numUnknowns) equations
      constants = LA.vector $ map getConstant equations
   in (matrix, constants)
  where
    -- Convert equation to a row in the matrix
    equationToRow :: Map.Map Unknown Int -> Int -> Equation -> [Double]
    equationToRow indices size (Equation lhs _) =
      let coeffPairs = termToCoefficients indices lhs
       in [if i `elem` map fst coeffPairs then Maybe.fromMaybe 0 (lookup i coeffPairs) else 0 | i <- [0 .. size - 1]]

    -- Extract coefficients from terms
    termToCoefficients :: Map.Map Unknown Int -> Term -> [(Int, Double)]
    termToCoefficients indices (Sum terms) = concatMap (termToCoefficients indices) terms
    termToCoefficients indices (Product terms) =
      case List.partition isConstant terms of
        (constants, [UnknownTerm u]) -> [(indices Map.! u, product [c | Constant c <- constants])]
        _ -> []
    termToCoefficients indices (UnknownTerm u) = [(indices Map.! u, 1.0)]
    termToCoefficients _ (Constant _) = []

    -- Get the constant term (right hand side) by combining constant terms
    getConstant :: Equation -> Double
    getConstant (Equation _ (Constant c)) = c
    getConstant (Equation _ (Sum terms)) = sum (map getConstant' terms)
    getConstant (Equation _ (Product terms)) = product (map getConstant' terms)
    getConstant (Equation _ (UnknownTerm _)) = error "Unknown term in constant position"

    getConstant' :: Term -> Double
    getConstant' (Constant c) = c
    getConstant' (Sum terms) = sum (map getConstant' terms)
    getConstant' (Product terms) = product (map getConstant' terms)
    getConstant' (UnknownTerm _) = 0

    -- Helper to identify constant terms
    isConstant :: Term -> Bool
    isConstant (Constant _) = True
    isConstant _ = False

solve :: Circuit -> Map.Map Unknown Double
solve circuit =
  let equations = getEquations circuit
      (matrix, constants) = equationsToMatrix equations
      solution = LA.linearSolveSVD matrix (LA.asColumn constants)
      unknowns = getUnknowns equations
   in Map.fromList $ zip (Set.toList unknowns) (LA.toList (LA.flatten solution))

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

getUnknowns :: [Equation] -> Set.Set Unknown
getUnknowns = Set.fromList . concatMap getEquationUnknowns
  where
    getEquationUnknowns (Equation lhs _) = getTermUnknowns lhs

    getTermUnknowns (Sum terms) = concatMap getTermUnknowns terms
    getTermUnknowns (Product terms) = concatMap getTermUnknowns terms
    getTermUnknowns (UnknownTerm u) = [u]
    getTermUnknowns (Constant _) = []

getUnknownsWithIndices :: [Equation] -> Map.Map Unknown Int
getUnknownsWithIndices eqns =
  Map.fromList $ zip (Set.toList unknowns) [0 ..]
  where
    unknowns = getUnknowns eqns
