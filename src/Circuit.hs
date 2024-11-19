module Circuit (
  Circuit,
  Node,
  Component,
  ComponentType,
  Var,
  Unknown
) where

import Prelude
import Data.Map

data Unknown
  = NodeVoltage String -- for nodes
  | Parameter String -- for components
  deriving (Show, Eq)

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
  { componentID :: String,
    componentType :: ComponentType,
    current :: Var,
    nodePos :: String, -- node ID
    nodeNeg :: String -- node ID
  }
  deriving (Show, Eq)

data Node = Node
  { nID :: String,
    nodeVoltage :: Var
  }

data Circuit = Circuit
  { nodes :: Map String Node,
    components :: Map String Component
  }

-- validation function
-- gets rid of any floating nodes
-- fails if components use a node that doesn't exist
validate :: Circuit -> Maybe Circuit
validate = undefined