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
    nodes,
    components,
    componentType,
    current,
    nodeVoltage,
  )
where

import Control.Monad (when)
import Data.List qualified as List
import Data.Map
import Data.Map qualified as Map
import Data.Text (pack, strip, unpack)
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
  deriving (Eq)

instance Show Component where
  show comp =
    let (ComponentID cid) = componentID comp
        (NodeID pos) = nodePos comp
        (NodeID neg) = nodeNeg comp
        curr = case current comp of
          Known v -> show v
          Unknown _ -> "?"
        typeStr = case componentType comp of
          VSource _ -> "V"
          Resistor _ -> "R"
     in typeStr ++ " " ++ cid ++ " (-): " ++ neg ++ " (+): " ++ pos ++ " I: " ++ curr

data Node = Node
  { nodeID :: NodeID,
    nodeVoltage :: Var
  }
  deriving (Eq)

instance Show Node where
  show node =
    let (NodeID nid) = nodeID node
        v = case nodeVoltage node of
          Known val -> show val
          Unknown _ -> "?"
     in nid ++ " V: " ++ v

data Circuit = Circuit
  { nodes :: Map NodeID Node,
    components :: Map ComponentID Component
  }
  deriving (Eq)

instance Show Circuit where
  show (Circuit nodes components) =
    "Circuit {\n"
      ++ "  Nodes:\n"
      ++ showNodes (Map.toList nodes)
      ++ "  Components:\n"
      ++ showComponents (Map.toList components)
      ++ "}"
    where
      showNodes [] = ""
      showNodes ns = concatMap (\(_, n) -> "    " ++ show n ++ "\n") ns

      showComponents [] = ""
      showComponents cs = concatMap (\(_, c) -> "    " ++ show c ++ "\n") cs