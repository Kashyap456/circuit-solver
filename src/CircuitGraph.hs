module CircuitGraph
  ( CircuitTopology (..),
    SearchState (..),
    buildTopology,
    findLoopsFromNode,
    isValidNextNode,
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
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set

-- TODO: Add deduplication logic for loops
-- TODO: treat paths as backward to avoid concat operations

-- Precomputed adjacency information
newtype CircuitTopology = CircuitTopology
  {adjacencyMap :: Map.Map NodeID [(NodeID, ComponentID)]}

-- Build the topology once, use it many times
buildTopology :: Circuit -> CircuitTopology
buildTopology circuit =
  let -- Build adjacency map
      adjList = Map.foldr (addComponentEdges (components circuit)) Map.empty (components circuit)
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
          -- Add bidirectional edges
          addEdge n1 n2 = Map.insertWith (++) n1 [(n2, cid)]
       in addEdge pos neg . addEdge neg pos

-- Optimized search state
data SearchState = SearchState
  { currentNode :: NodeID,
    usedComponents :: Set ComponentID,
    pathNodes :: [NodeID] -- Store just nodes, components can be looked up later
  }

-- Find all loops starting from a node using precomputed topology
findLoopsFromNode :: Circuit -> CircuitTopology -> NodeID -> [Circuit]
findLoopsFromNode circuit topo startNode =
  findLoopsFromState circuit topo initialState
  where
    initialState =
      SearchState
        { currentNode = startNode,
          usedComponents = Set.empty,
          pathNodes = []
        }

-- Core loop finding using precomputed adjacency
findLoopsFromState :: Circuit -> CircuitTopology -> SearchState -> [Circuit]
findLoopsFromState circuit topo state@(SearchState current used path) =
  let completePath = path ++ [current]
   in maybeToList (pathToCircuit circuit completePath)
        ++ (nextMoves topo state >>= findLoopsFromState circuit topo)

-- Get next possible moves using precomputed adjacency
nextMoves :: CircuitTopology -> SearchState -> [SearchState]
nextMoves topo (SearchState current used path) =
  [ SearchState nextNode (Set.insert compId used) (path ++ [current])
    | (nextNode, compId) <- Map.findWithDefault [] current (adjacencyMap topo),
      not (Set.member compId used),
      isValidNextNode nextNode (path ++ [current])
  ]

-- Check if a node can be added to the path
isValidNextNode :: NodeID -> [NodeID] -> Bool
isValidNextNode node path = case path of
  [] -> True -- Empty path - any node valid
  [n] -> node /= n -- Single node - can't revisit
  (start : rest) ->
    node `notElem` rest -- Can't revisit intermediate nodes
      && (node /= start || not (null rest)) -- Can return to start if path long enough

-- Convert a valid path to a circuit using precomputed maps
pathToCircuit :: Circuit -> [NodeID] -> Maybe Circuit
pathToCircuit circuit path = case path of
  [] -> Nothing
  [_] -> Nothing
  nodes@(first : rest)
    | last nodes == first && not (null rest) -> Just $ makeLoopCircuit circuit nodes
    | otherwise -> Nothing

-- Construct a circuit from a path using precomputed maps
makeLoopCircuit :: Circuit -> [NodeID] -> Circuit
makeLoopCircuit circuit nodePath =
  let nodeSet = Set.fromList nodePath
      loopNodes = Map.restrictKeys (nodes circuit) nodeSet
      loopComponents =
        zip nodePath (tail nodePath)
          & concatMap (getConnectingComponent circuit)
          & Map.fromList
   in Circuit
        { nodes = loopNodes,
          components = loopComponents
        }

connectsNodes' :: NodeID -> NodeID -> Component -> Bool
connectsNodes' n1 n2 comp =
  (nodePos comp == n1 && nodeNeg comp == n2)
    || (nodePos comp == n2 && nodeNeg comp == n1)

-- Find component connecting two nodes using precomputed adjacency
getConnectingComponent :: Circuit -> (NodeID, NodeID) -> [(ComponentID, Component)]
getConnectingComponent circuit (n1, n2) =
  [ (componentID comp, comp)
    | comp <- Map.elems (components circuit),
      connectsNodes' n1 n2 comp
  ]