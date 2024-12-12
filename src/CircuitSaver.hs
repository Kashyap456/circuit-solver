module Circuit 
    (
        parseCircuit,
        saveCircuit
    )
where
import Circuit
import YamlParser
import Data.Map (Map, fromList, foldrWithKey, notMember, lookup)
import Control.Monad (when)
import GHC.IO.IOMode (IOMode (WriteMode))
import System.FilePath (takeExtension)
import System.IO
import Data.Map qualified as Map
import qualified Data.List as List
import Prelude hiding (lookup)

-- Create circuit from a YAML file
parseCircuit :: (Ord NodeID, Ord ComponentID) => String -> IO (Maybe Circuit)
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

--- TO-STRING FUNCTIONS ---

-- Convert a circuit to its YAML file
circuitToString :: Circuit -> String
circuitToString circuit =
  let nodeMap = nodes circuit
   in let comps = components circuit
       in nodesToString nodeMap ++ componentsToString comps

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

--- GETTERS ---

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

getNodes :: (Ord NodeID) => Map String YAMLValue -> Maybe (Map NodeID Node)
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

getComponents :: (Ord ComponentID) => Map String YAMLValue -> Maybe (Map ComponentID Component)
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


getCircuit :: (Ord NodeID, Ord ComponentID) => Map String YAMLValue -> Maybe Circuit
getCircuit m = do
  nodes <- getNodes m
  components <- getComponents m
  pure (Circuit nodes components)