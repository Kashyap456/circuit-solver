module CircuitGraph
  ( CircuitTopology (..),
    SearchState (..),
    buildTopology,
    findLoopsFromNode,
    pathToCircuit,
    makeLoopCircuit,
    nextMoves,
  )
where

import Circuit
  ( Circuit (..),
    Component (..),
    ComponentID (..),
    Node (..),
    NodeID (..),
  )
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Path (Path (..), PathContent (..), PathRest (..), addPair, emptyPath, isLoop, pathComponents, pathNodes, startPath)

-- Precomputed adjacency information
newtype CircuitTopology = CircuitTopology
  {adjacencyMap :: Map.Map NodeID [(NodeID, ComponentID)]}

-- Build the topology once, use it many times
buildTopology :: Circuit -> CircuitTopology
buildTopology circuit =
  let adjList = Map.foldr (addComponentEdges (components circuit)) Map.empty (components circuit)
   in CircuitTopology {adjacencyMap = adjList}
  where
    addComponentEdges ::
      Map.Map ComponentID Component ->
      Component ->
      Map.Map NodeID [(NodeID, ComponentID)] ->
      Map.Map NodeID [(NodeID, ComponentID)]
    addComponentEdges compMap comp =
      let cid = componentID comp
          pos = nodePos comp
          neg = nodeNeg comp
          addEdge n1 n2 = Map.insertWith (++) n1 [(n2, cid)]
       in addEdge pos neg . addEdge neg pos

-- Optimized search state using Path
data SearchState = SearchState
  { currentPath :: Path,
    usedComponents :: Set ComponentID
  }
  deriving (Show)

-- Find all loops starting from a node using precomputed topology
findLoopsFromNode :: Circuit -> CircuitTopology -> NodeID -> [Circuit]
findLoopsFromNode circuit topo startNode =
  let allLoops = findLoopsFromState circuit topo initialState
      -- Create pairs of (sorted component IDs, circuit) for deduplication
      loopsWithKeys = [(Set.fromList $ Map.keys $ components c, c) | c <- allLoops]
      -- Use Map to keep only unique component combinations
      uniqueLoops = Map.elems $ Map.fromList loopsWithKeys
   in uniqueLoops
  where
    initialState =
      SearchState
        { currentPath = startPath startNode,
          usedComponents = Set.empty
        }

-- Core loop finding using precomputed adjacency
findLoopsFromState :: Circuit -> CircuitTopology -> SearchState -> [Circuit]
findLoopsFromState circuit topo state =
  maybeToList
    ( if isLoop (currentPath state)
        then pathToCircuit circuit (currentPath state)
        else Nothing
    )
    ++ (nextMoves topo state >>= findLoopsFromState circuit topo)

-- Get next possible moves using precomputed adjacency
nextMoves :: CircuitTopology -> SearchState -> [SearchState]
nextMoves (CircuitTopology adjMap) (SearchState path used) =
  case path of
    Path start (ConsNode currentNode _) ->
      -- Get all adjacent nodes and their connecting components
      case Map.lookup currentNode adjMap of
        Nothing -> []
        Just adjacentNodes ->
          -- Filter out used components and create new states
          [ SearchState newPath (Set.insert componentId used)
            | (nextNode, componentId) <- adjacentNodes,
              not (Set.member componentId used),
              Just newPath <- [addPair nextNode componentId path]
          ]
    _ -> [] -- Empty path or invalid state

-- Convert a valid path to a circuit
pathToCircuit :: Circuit -> Path -> Maybe Circuit
pathToCircuit circuit path
  | isLoop path = Just $ makeLoopCircuit circuit path
  | otherwise = Nothing

-- Construct a circuit from a path
makeLoopCircuit :: Circuit -> Path -> Circuit
makeLoopCircuit circuit path =
  let nodeList = pathNodes path
      nodeSet = Set.fromList nodeList
      loopNodes = Map.restrictKeys (nodes circuit) nodeSet
      componentList = pathComponents path
      loopComponents = Map.restrictKeys (components circuit) (Set.fromList componentList)
   in Circuit
        { nodes = loopNodes,
          components = loopComponents
        }

connectsNodes' :: NodeID -> NodeID -> Component -> Bool
connectsNodes' n1 n2 comp =
  (nodePos comp == n1 && nodeNeg comp == n2)
    || (nodePos comp == n2 && nodeNeg comp == n1)

-- Find component connecting two nodes
getConnectingComponent :: Circuit -> (NodeID, NodeID) -> [(ComponentID, Component)]
getConnectingComponent circuit (n1, n2) =
  [ (componentID comp, comp)
    | comp <- Map.elems (components circuit),
      connectsNodes' n1 n2 comp
  ]