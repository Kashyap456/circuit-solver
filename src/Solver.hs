module Solver where

import Circuit (Circuit (..), Component (..), ComponentID, ComponentType (..), Node (..), NodeID, Unknown (..), Var (..), componentType)
import CircuitGraph (CircuitTopology, buildTopology, findLoopPathsFromNode)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Numeric.LinearAlgebra qualified as LA
import Path (Path, pathComponents, pathNodes)

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

newtype UsedComponents = UsedComponents (Set.Set ComponentID)

data SearchState = SearchState
  { currentNode :: NodeID,
    usedComponents :: UsedComponents,
    pathSoFar :: Path
  }

-- Simplify the current equation if possible
simplify :: Circuit -> Circuit
simplify = undefined

loopToKVLEquation :: Circuit -> Path -> Equation
loopToKVLEquation circuit path =
  let nodeList = pathNodes path
      compList = pathComponents path
      -- Zip components with their directions based on path traversal
      compDirs = zipWith (\c n2 -> isForwardTraversal c n2) compList (tail nodeList)
      -- Get the components from the main circuit
      comps = [Maybe.fromJust $ Map.lookup cid (components circuit) | cid <- compList]
      -- Create voltage terms with proper directions
      terms = zipWith (componentVoltage circuit) compDirs comps
      -- Separate constants and variables
      (constants, variables) = List.partition isConstantTerm terms
      -- Sum up all constants
      constantSum = case constants of
        [] -> Constant 0.0
        _ -> Sum constants
   in Equation {lhs = Sum variables, rhs = Product [Constant (-1), constantSum]}
  where
    isForwardTraversal :: ComponentID -> NodeID -> Bool
    isForwardTraversal cid nextNode =
      let comp = Maybe.fromJust $ Map.lookup cid (components circuit)
       in nodeNeg comp == nextNode

    isConstantTerm :: Term -> Bool
    isConstantTerm (Constant _) = True
    isConstantTerm _ = False

nodeToKCLEquation :: Circuit -> Node -> Equation
nodeToKCLEquation circuit node =
  Equation
    { lhs =
        Sum
          [ let isPositiveTerminal = nodePos comp == nodeID node
                currentTerm = componentCurrent True comp
             in case componentType comp of
                  VSource _ ->
                    if isPositiveTerminal
                      then currentTerm -- Current enters at positive terminal
                      else Product [Constant (-1), currentTerm]
                  Resistor _ ->
                    if isPositiveTerminal
                      then Product [Constant (-1), currentTerm] -- Current leaves at positive terminal
                      else currentTerm
            | comp <- Map.elems (components circuit),
              nodePos comp == nodeID node || nodeNeg comp == nodeID node
          ],
      rhs = Constant 0.0
    }

getKVLEquations :: Circuit -> CircuitTopology -> [Equation]
getKVLEquations circuit topology =
  let paths =
        List.nubBy
          (\p1 p2 -> Set.fromList (pathComponents p1) == Set.fromList (pathComponents p2))
          [p | nodeId <- Map.keys (nodes circuit), p <- findLoopPathsFromNode circuit topology nodeId]
   in map (loopToKVLEquation circuit) paths

getKCLEquations :: Circuit -> CircuitTopology -> [Equation]
getKCLEquations circuit topology =
  let equationNodes = Map.elems (nodes circuit)
   in map (nodeToKCLEquation circuit) equationNodes

getOhmEquations :: Circuit -> [Equation]
getOhmEquations circuit =
  [ Equation
      { lhs =
          Sum
            [ nodeVoltageDiff,
              Product [Constant (-resistance), UnknownTerm (Parameter curr)]
            ],
        rhs = Constant 0.0
      }
    | comp <- Map.elems (components circuit),
      let maybeValues = case componentType comp of
            Resistor (Known r) ->
              case current comp of
                Unknown (Parameter c) ->
                  let posNode = Maybe.fromJust $ Map.lookup (nodePos comp) (nodes circuit)
                      negNode = Maybe.fromJust $ Map.lookup (nodeNeg comp) (nodes circuit)
                      nodeVoltageDiff =
                        Sum
                          [ case nodeVoltage posNode of
                              Known val -> Constant val
                              Unknown u -> UnknownTerm u,
                            Product
                              [ Constant (-1),
                                case nodeVoltage negNode of
                                  Known val -> Constant val
                                  Unknown u -> UnknownTerm u
                              ]
                          ]
                   in Just (r, c, nodeVoltageDiff)
                _ -> Nothing
            _ -> Nothing,
      Just (resistance, curr, nodeVoltageDiff) <- [maybeValues]
  ]

-- Modify getEquations to include Ohm's law
getEquations :: Circuit -> [Equation]
getEquations circuit =
  let topology = buildTopology circuit
      -- Get KVL equations from loops
      kvlEquations = getKVLEquations circuit topology
      -- Get KCL equations from nodes
      kclEquations = getKCLEquations circuit topology
      -- Get component equations (voltage sources, etc)
      componentEquations = getComponentEquations circuit
      -- Get Ohm's law equations
      ohmEquations = getOhmEquations circuit
   in kvlEquations ++ kclEquations ++ componentEquations ++ ohmEquations

-- helper function to generate voltage source equations
-- TODO: needs to handle cases where a constant ends up on the LHS here
getComponentEquations :: Circuit -> [Equation]
getComponentEquations circuit =
  [ Equation
      ( Sum
          [ case nodeVoltage $ Maybe.fromJust $ Map.lookup (nodePos comp) (nodes circuit) of
              Known v -> Constant v
              Unknown u -> UnknownTerm u,
            Product
              [ Constant (-1),
                case nodeVoltage $ Maybe.fromJust $ Map.lookup (nodeNeg comp) (nodes circuit) of
                  Known v -> Constant v
                  Unknown u -> UnknownTerm u
              ]
          ]
      )
      ( case componentType comp of
          VSource (Known v) -> Constant v
          VSource (Unknown u) -> UnknownTerm u
          _ -> Constant 0
      )
    | comp <- Map.elems (components circuit),
      VSource _ <- [componentType comp]
  ]

-- Add this as a top-level function
equationToRow :: Map.Map Unknown Int -> Int -> Equation -> [Double]
equationToRow indices size (Equation lhs _) =
  let coeffPairs = termToCoefficients indices lhs
   in [if i `elem` map fst coeffPairs then Maybe.fromMaybe 0 (lookup i coeffPairs) else 0 | i <- [0 .. size - 1]]

equationsToMatrix :: [Equation] -> (LA.Matrix Double, LA.Vector Double)
equationsToMatrix equations =
  let unknownIndices = getUnknownsWithIndices equations
      numUnknowns = Map.size unknownIndices
      numEquations = length equations
      -- Create matrix of coefficients and vector of constants
      coefficients = concatMap (equationToRow unknownIndices numUnknowns) equations
      matrix = LA.matrix numUnknowns coefficients
      constants = LA.vector $ map getConstant equations
   in (matrix, constants)

solve :: Circuit -> Map.Map Unknown Double
solve circuit =
  let equations = getEquations circuit
      (matrix, constants) = equationsToMatrix equations
      solution = LA.linearSolveLS matrix (LA.asColumn constants)
      unknowns = getUnknowns equations
   in Map.fromList $ zip (Set.toList unknowns) (LA.toList (LA.flatten solution))

componentVoltage :: Circuit -> Bool -> Component -> Term
componentVoltage circuit isForward comp =
  case componentType comp of
    VSource v -> case v of
      Known val -> if isForward then Constant val else Constant (-val)
      Unknown u -> if isForward then UnknownTerm u else Product [Constant (-1), UnknownTerm u]
    Resistor r -> case r of
      Known resistance ->
        case current comp of
          Known curr ->
            if isForward
              then Constant (resistance * curr)
              else Constant (-(resistance * curr))
          Unknown (Parameter cid) ->
            if isForward
              then Product [Constant resistance, UnknownTerm (Parameter cid)]
              else Product [Constant (-resistance), UnknownTerm (Parameter cid)]
          _ -> Constant 0.0
      Unknown u -> UnknownTerm u

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

-- Get the constant term (right hand side) by combining constant terms
getConstant :: Equation -> Double
getConstant (Equation _ rhs) = getConstant' rhs

-- Helper to identify constant terms
isConstant :: Term -> Bool
isConstant (Constant _) = True
isConstant (Product ts) = all isConstant ts
isConstant _ = False

-- Helper to evaluate constant terms
getConstant' :: Term -> Double
getConstant' (Constant c) = c
getConstant' (Sum terms) = sum (map getConstant' terms)
getConstant' (Product terms) = product (map getConstant' terms)
getConstant' (UnknownTerm _) = 0

termToCoefficients :: Map.Map Unknown Int -> Term -> [(Int, Double)]
termToCoefficients indices term = case term of
  Sum terms ->
    -- Combine coefficients for the same unknown
    Map.toList $ Map.fromListWith (+) $ concatMap (termToCoefficients indices) terms
  Product terms ->
    -- For products, multiply the constant terms and combine with the unknown
    let (constants, unknowns) = List.partition isConstant terms
        constProduct = product [c | Constant c <- constants]
     in case unknowns of
          [UnknownTerm u] -> [(indices Map.! u, constProduct)]
          _ -> [] -- Products of multiple unknowns not supported
  UnknownTerm u ->
    [(indices Map.! u, 1.0)]
  Constant _ ->
    []
