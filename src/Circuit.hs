module Circuit
  ( Circuit (Circuit),
    Node (Node),
    Component (Component),
    ComponentType (..),
    Var (..),
    Unknown (..),
    NodeID (..),
    ComponentID (..),
    nodeID,
    nodePos,
    nodeNeg,
    componentID,
    validate,
    nodes,
    components,
  )
where

import Data.Map
import Data.List qualified as List
import Data.Text (strip, pack, unpack)
import Prelude hiding (lookup)
import YamlParser (parseYAMLFile, YAMLValue (..), extractList, extractMap, extractString)

newtype NodeID = NodeID String deriving (Show, Eq, Ord)

newtype ComponentID = ComponentID String deriving (Show, Eq, Ord)

data Unknown
  = NodeVoltage NodeID -- for nodes
  | Parameter ComponentID -- for components
  deriving (Show, Eq, Ord)

data Var
  = Known Double
  | Unknown Unknown
  deriving (Show, Eq)

data ComponentType
  = VSource {voltage :: Var}
  | Resistor {resistance :: Var}
  deriving (Show, Eq)

wire :: ComponentType
wire = Resistor (Known 0.0)

data Component = Component
  { componentID :: ComponentID,
    componentType :: ComponentType,
    current :: Var,
    nodePos :: NodeID,
    nodeNeg :: NodeID
  }
  deriving (Show, Eq)

data Node = Node
  { nodeID :: NodeID,
    nodeVoltage :: Var
  }
  deriving (Show, Eq)

data Circuit = Circuit
  { nodes :: Map NodeID Node,
    components :: [Component]
  }
  deriving (Show, Eq)

-- validation function
-- gets rid of any floating nodes
-- fails if components use a node that doesn't exist
validate :: Circuit -> Maybe Circuit
validate = undefined

-- Create circuit from a YAML file
parseCircuit :: String -> IO (Maybe Circuit)
parseCircuit filename = do
  parsedYAML <- parseYAMLFile filename
  case parsedYAML of
    Right (YAMLMap m) -> return (getCircuit m)
    _ -> return Nothing

saveCircuit :: String -> Circuit -> IO ()
saveCircuit = undefined

-- Helper for getting a Var type from a map
getVar :: Map String YAMLValue -> String -> Maybe Var
getVar m paramName = case lookup paramName m of
  Just (YAMLString s) -> Just (Unknown (Parameter (ComponentID s)))
  Just (YAMLDouble d) -> Just (Known d)
  _ -> Nothing

-- Given a map of the component
getComponentType :: Map String YAMLValue -> Maybe ComponentType
getComponentType m = do
  id <- lookup "id" m >>= extractString
  t <- lookup "type" m >>= extractString
  (case t of
    "voltage" -> f "voltage" VSource
    "resistor" -> f "resistance" Resistor
    _ -> Nothing) where
      f paramName constructor = do
        var <- getVar m paramName
        pure (constructor var)

-- Gets a single node
getNode :: Map String YAMLValue -> Maybe Node
getNode m = do
  id <- lookup "id" m >>= extractString
  voltage <- getVar m "voltage"
  pure (Node (NodeID id) voltage)

getNodes :: Map String YAMLValue -> Maybe (Map NodeID Node)
getNodes m =
  if notMember "nodes" m then Nothing
  else do
    l <- lookup "nodes" m >>= extractList
    return (fromList (List.foldr f [] l)) where
      f x acc = case extractMap x of
        Just m -> case getNode m of
          Just n -> (nodeID n, n) : acc
          _ -> acc
        Nothing -> acc

-- Gets a single component
getComponent :: Map String YAMLValue -> Maybe Component
getComponent m = do
  id <- lookup "id" m >>= extractString
  current <- getVar m "current"
  pos <- lookup "pos" m >>= extractString
  neg <- lookup "neg" m >>= extractString
  t <- getComponentType m
  pure (Component (ComponentID id) t current (NodeID pos) (NodeID neg))

getComponents :: Map String YAMLValue -> Maybe [Component]
getComponents m =
  if notMember "components" m then Nothing
  else do
    l <- lookup "components" m >>= extractList
    return (List.foldr f [] l) where
      f x acc = case extractMap x of
        Just m -> case getComponent m of
          Just c -> c : acc
          _ -> acc
        Nothing -> acc

getCircuit :: Map String YAMLValue -> Maybe Circuit
getCircuit m = do
  nodes <- getNodes m
  components <- getComponents m
  pure (Circuit nodes components)
