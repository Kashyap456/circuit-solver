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
  )
where

import Data.Map
import Prelude

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
validate = undefined

-- Create circuit from a YAML file
parseCircuit :: String -> Maybe Circuit
parseCircuit = undefined