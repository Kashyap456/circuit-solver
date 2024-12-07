module Path where

import Circuit (ComponentID, NodeID)
import Data.List qualified as List
import Data.Maybe (isJust)

-- Path representation with stored start node for O(1) loop checking
-- The path is stored in reverse order, with the most recent node first
data Path = Path
  { pathStart :: Maybe NodeID, -- Nothing for empty path
    pathContent :: PathContent
  }

instance Show Path where
  show (Path _ Empty) = "Empty Path"
  show (Path _ content) = showContent content
    where
      showContent :: PathContent -> String
      showContent Empty = ""
      showContent (ConsNode nid PathEnd) = show nid
      showContent (ConsNode nid (ConsComp cid rest)) =
        show nid ++ "->" ++ show cid ++ "->" ++ showContent rest

-- PathContent now starts with the most recent node
data PathContent
  = ConsNode NodeID PathRest -- Path always ends with a node
  | Empty
  deriving (Show, Eq)

-- After a node, we can have either nothing or a component followed by more path
data PathRest
  = PathEnd
  | ConsComp ComponentID PathContent
  deriving (Show, Eq)

-- Smart constructors and helpers
emptyPath :: Path
emptyPath = Path Nothing Empty

startPath :: NodeID -> Path
startPath nid = Path (Just nid) (ConsNode nid PathEnd)

-- Type-safe insertion function for node-component pairs
addPair :: NodeID -> ComponentID -> Path -> Maybe Path
addPair nid cid (Path start rest) =
  Just $ Path start (ConsNode nid (ConsComp cid rest))

-- Helper to get all nodes in path order (reversed from storage order)
pathNodes :: Path -> [NodeID]
pathNodes (Path _ Empty) = []
pathNodes (Path _ content) = reverse $ collectNodes content
  where
    collectNodes :: PathContent -> [NodeID]
    collectNodes Empty = []
    collectNodes (ConsNode nid PathEnd) = [nid]
    collectNodes (ConsNode nid (ConsComp _ rest)) = nid : collectNodes rest

-- Helper to get all components in path order (reversed from storage order)
pathComponents :: Path -> [ComponentID]
pathComponents (Path _ Empty) = []
pathComponents (Path _ content) = reverse $ collectComponents content
  where
    collectComponents :: PathContent -> [ComponentID]
    collectComponents Empty = []
    collectComponents (ConsNode _ PathEnd) = []
    collectComponents (ConsNode _ (ConsComp cid rest)) = cid : collectComponents rest

-- O(1) check if path forms a loop
isLoop :: Path -> Bool
isLoop (Path Nothing _) = False
isLoop (Path (Just start) content) = case getLastNode content of
  Nothing -> False
  Just end -> start == end && not (null $ pathComponents (Path Nothing content))
  where
    getLastNode :: PathContent -> Maybe NodeID
    getLastNode Empty = Nothing
    getLastNode (ConsNode nid _) = Just nid

-- Validation helpers
isValid :: Path -> Bool
isValid path = validatePath path && (not . hasDuplicateNodes) path

validatePath :: Path -> Bool
validatePath (Path start content) = case start of
  Nothing -> content == Empty
  Just s -> case content of
    Empty -> False
    ConsNode nid _ -> s == nid

hasDuplicateNodes :: Path -> Bool
hasDuplicateNodes path =
  let nodes = pathNodes path
      len = length nodes
      uniqueLen = length (List.nub nodes)
   in len /= uniqueLen