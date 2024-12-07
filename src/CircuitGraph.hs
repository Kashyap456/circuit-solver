module CircuitGraph
  ( CircuitTopology (..),
    SearchState (..),
    buildTopology,
    makeLoopCircuit,
    nextMoves,
    findLoopPathsFromNode,
    findLoopPathsFromState,
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

-- TODO: use the state Monad instead of explicitly passing the search state around

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

-- Find all loop paths starting from a node using precomputed topology
findLoopPathsFromNode :: Circuit -> CircuitTopology -> NodeID -> [Path]
findLoopPathsFromNode circuit topo startNode =
  let allLoops = findLoopPathsFromState topo initialState
      -- Create pairs of (sorted component IDs, path) for deduplication
      loopsWithKeys = [(Set.fromList $ pathComponents p, p) | p <- allLoops]
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
findLoopPathsFromState :: CircuitTopology -> SearchState -> [Path]
findLoopPathsFromState topo state =
  ([currentPath state | isLoop (currentPath state)])
    ++ (nextMoves topo state >>= findLoopPathsFromState topo)

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