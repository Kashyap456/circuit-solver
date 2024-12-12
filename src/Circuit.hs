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
    nodeVoltage
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