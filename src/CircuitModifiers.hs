module CircuitModifiers
    (
        addNode,
        addComponent,
        deleteNode,
        deleteComponent,
        updateNodeVoltage,
        validate
    )
where

import Data.Map
import Data.Unique
import Circuit


-- Generates a unique ID
generateID :: (Ord a) => String -> (String -> a) -> Map a b -> IO a
generateID s fn m = do
  unique <- newUnique
  let id = hashUnique unique
   in let idStr = s ++ show id
       in if member (fn idStr) m then generateID s fn m else pure $ fn idStr

-- Generates a component id
generateComponentID :: ComponentType -> Map ComponentID Component -> IO ComponentID
generateComponentID t m =
  let prefix = case t of
        VSource _ -> "v_"
        Resistor _ -> "i_"
   in do
        generateID prefix ComponentID m

--- Add Elements ---
addNode :: Circuit -> Var -> IO Circuit
addNode c voltage = do
  id <- generateID "n" NodeID (nodes c)
  return (Circuit (insert id (Node id voltage) (nodes c)) (components c))

addComponent :: Circuit -> Var -> NodeID -> NodeID -> ComponentType -> IO Circuit
addComponent c current pos neg t = do
  id <- generateComponentID t (components c)
  return (Circuit (nodes c) (insert id (Component id t current pos neg) (components c)))


--- Delete Elements ---
deleteNode :: Circuit -> NodeID -> Circuit
deleteNode c id = Circuit (delete id (nodes c)) (components c)

deleteComponent :: Circuit -> ComponentID -> Circuit
deleteComponent c id = Circuit (nodes c) (delete id (components c))

--- Update variables ---
updateNodeVoltage :: Circuit -> NodeID -> Var -> Circuit
updateNodeVoltage c id v = 
  if member id (nodes c) then
    Circuit (insert id (Node id v) (nodes c)) (components c) 
    else c

-- Remove floating nodes. If a component uses a node that doesn't exist, return Nothing.
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
