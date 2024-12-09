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
    componentType,
    current,
    nodeVoltage,
    parseCircuit,
    saveCircuit,
    addNode,
    deleteNode,
    addComponent,
    deleteComponent
  )
where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map
import Data.Map qualified as Map
import Data.Text (pack, strip, unpack)
import Data.Unique
import GHC.IO.IOMode (IOMode (WriteMode))
import System.FilePath (takeExtension)
import System.IO
import YamlParser (YAMLValue (..), extractList, extractMap, extractString, parseYAMLFile)
import Prelude hiding (lookup)

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
    components :: Map ComponentID Component
  }
  deriving (Show, Eq)

-- validation function
-- gets rid of any floating nodes
-- fails if components use a node that doesn't exist
validate :: Circuit -> Maybe Circuit
validate c = do
  let freqMap = componentsToFreqMap c in
    let n = nodes c in
      if Map.null (filterWithKey (\id _ -> notMember id n) freqMap) 
        then Just (Circuit (filterWithKey (\id _ -> member id freqMap) n) (components c)) 
        else Nothing
  

-- Returns a frequency map for each node to its number of connected components in the circuit
componentsToFreqMap :: Circuit -> Map NodeID Int
componentsToFreqMap c =
  let compList = elems (components c)
   in List.foldr f Map.empty compList
  where
    f :: Component -> Map NodeID Int -> Map NodeID Int
    f x acc = do
      let m = insert (nodePos x) (findWithDefault 0 (nodePos x) acc + 1) acc
       in insert (nodeNeg x) (findWithDefault 0 (nodeNeg x) m + 1) m

-- Create circuit from a YAML file
parseCircuit :: String -> IO (Maybe Circuit)
parseCircuit filename = do
  parsedYAML <- parseYAMLFile filename
  case parsedYAML of
    Right (YAMLMap m) -> return (getCircuit m)
    _ -> return Nothing

-- Saves a circuit to a YAML file
saveCircuit :: String -> Circuit -> IO ()
saveCircuit filename circuit = do
  when (takeExtension filename /= ".yaml") $ ioError (userError "File should be a yaml file")
  writeFile filename (circuitToString circuit)
  return ()

-- ID Generators
generateID :: (Ord a) => String -> (String -> a) -> Map a b -> IO a
generateID s fn m = do
  unique <- newUnique
  let id = hashUnique unique
   in let idStr = s ++ show id
       in if member (fn idStr) m then generateID s fn m else pure $ fn idStr

--- Circuit Modifiers ---
addNode :: Circuit -> Var -> IO Circuit
addNode c voltage = do
  id <- generateID "n" NodeID (nodes c)
  return (Circuit (insert id (Node id voltage) (nodes c)) (components c))

deleteNode :: Circuit -> NodeID -> Circuit
deleteNode c id = Circuit (delete id (nodes c)) (components c)

addComponent :: Circuit -> Var -> NodeID -> NodeID -> ComponentType -> IO Circuit
addComponent c current pos neg t = do
  id <- getComponentID t (components c)
  return (Circuit (nodes c) (insert id (Component id t current pos neg) (components c)))

deleteComponent :: Circuit -> ComponentID -> Circuit
deleteComponent c id = Circuit (nodes c) (delete id (components c))

getComponentID :: ComponentType -> Map ComponentID Component -> IO ComponentID
getComponentID t m =
  let prefix = case t of
        VSource _ -> "v_"
        Resistor _ -> "i_"
   in do
        generateID prefix ComponentID m

-- Convert a variable to a string
varToString :: Var -> String
varToString v = case v of
  Known d -> show d
  Unknown (NodeVoltage (NodeID v)) -> v
  Unknown (Parameter (ComponentID c)) -> c

-- Convert a circuit's nodes to its YAML string
nodesToString :: Map NodeID Node -> String
nodesToString nodeMap = "nodes:\n" ++ foldrWithKey f "" nodeMap
  where
    f (NodeID key) node acc =
      " - id: " ++ key ++ "\n   voltage: " ++ varToString (nodeVoltage node) ++ "\n" ++ acc

componentTypeToString :: ComponentType -> String
componentTypeToString t = case t of
  VSource voltage -> "   type: voltage\n   voltage: " ++ varToString voltage ++ "\n"
  Resistor resistance -> "   type: resistor\n   resistance: " ++ varToString resistance ++ "\n"

-- Convert a circuit's nodes to its YAML string
componentsToString :: Map ComponentID Component -> String
componentsToString components = "components:\n" ++ foldrWithKey f "" components
  where
    f compId comp acc =
      let (ComponentID id) = componentID comp
       in let (NodeID posID) = nodePos comp
           in let (NodeID negID) = nodeNeg comp
               in " - id: "
                    ++ id
                    ++ "\n   circuit: "
                    ++ varToString (current comp)
                    ++ "\n   pos: "
                    ++ posID
                    ++ "\n   neg: "
                    ++ negID
                    ++ "\n"
                    ++ componentTypeToString (componentType comp)
                    ++ acc

-- Convert a circuit to its YAML file
circuitToString :: Circuit -> String
circuitToString circuit =
  let nodeMap = nodes circuit
   in let comps = components circuit
       in nodesToString nodeMap ++ componentsToString comps

-- Get a Var from a YAMLMap
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
  ( case t of
      "voltage" -> f "voltage" VSource
      "resistor" -> f "resistance" Resistor
      _ -> Nothing
    )
  where
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
  if notMember "nodes" m
    then Nothing
    else do
      l <- lookup "nodes" m >>= extractList
      return (fromList (List.foldr f [] l))
  where
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

getComponents :: Map String YAMLValue -> Maybe (Map ComponentID Component)
getComponents m =
  if notMember "components" m
    then Nothing
    else do
      l <- lookup "components" m >>= extractList
      return (fromList (List.foldr f [] l))
  where
    f x acc = case extractMap x of
      Just m -> case getComponent m of
        Just c -> (componentID c, c) : acc
        _ -> acc
      Nothing -> acc

getCircuit :: Map String YAMLValue -> Maybe Circuit
getCircuit m = do
  nodes <- getNodes m
  components <- getComponents m
  pure (Circuit nodes components)