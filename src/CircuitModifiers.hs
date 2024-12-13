module CircuitModifiers
  ( addNode,
    addComponent,
    deleteNode,
    deleteComponent,
    updateNodeVoltage,
    updateComponentCurrent,
    updateComponentNodes,
    updateComponentValue,
    validate,
  )
where

import Circuit
import Data.List qualified as List
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique

-- Generates a unique ID
generateID :: (Ord a) => String -> (String -> a) -> Map a b -> IO a
generateID s fn m = do
  unique <- newUnique
  let id = hashUnique unique
   in let idStr = s ++ show id
       in if member (fn idStr) m then generateID s fn m else pure $ fn idStr

getPrefix :: Bool -> String
getPrefix isResistor = if isResistor then "r" else "v"

-- Generates a component id
generateComponentID :: Bool -> Map ComponentID Component -> IO ComponentID
generateComponentID isResistor = generateID (getPrefix isResistor) ComponentID

--- Add Elements ---
addNode :: Circuit -> Maybe NodeID -> Maybe Double -> IO Circuit
addNode c givenID voltage = do
  id <- case givenID of
    Just id -> pure id
    _ -> generateID "n" NodeID (nodes c)
  let v = case voltage of
        Just d -> Known d
        Nothing -> Unknown (NodeVoltage id)
   in return (Circuit (insert id (Node id v) (nodes c)) (components c))

createNodeIfDNE :: Circuit -> NodeID -> IO Circuit
createNodeIfDNE c id = if member id (nodes c) then pure c else addNode c (Just id) Nothing

addComponent :: Circuit -> NodeID -> NodeID -> Maybe Double -> Bool -> Maybe Double -> IO Circuit
addComponent c pos neg i isResistor value = do
  id@(ComponentID idStr) <- generateComponentID isResistor (components c)
  c' <- createNodeIfDNE c pos
  c'' <- createNodeIfDNE c' neg
  let v = case value of
        Just d -> Known d
        _ -> Unknown (Parameter (ComponentID (getPrefix isResistor ++ "_" ++ idStr)))
   in let t = (if isResistor then Resistor else VSource) v
       in return (Circuit (nodes c'') (insert id (Component id t (createCurrentVar id i) pos neg) (components c'')))

--- Create Vars ---
createCurrentVar :: ComponentID -> Maybe Double -> Var
createCurrentVar (ComponentID id) i =
  case i of
    Just d -> Known d
    Nothing -> Unknown (Parameter (ComponentID ("i_" ++ id)))

--- Delete Elements ---
deleteNode :: Circuit -> NodeID -> Circuit
deleteNode c id = Circuit (delete id (nodes c)) (components c)

deleteComponent :: Circuit -> ComponentID -> Circuit
deleteComponent c id = Circuit (nodes c) (delete id (components c))

--- Update variables ---
updateNodeVoltage :: Circuit -> NodeID -> Maybe Double -> Circuit
updateNodeVoltage c id voltage =
  if member id (nodes c)
    then
      let v = case voltage of
            Just d -> Known d
            Nothing -> Unknown (NodeVoltage id)
       in Circuit (insert id (Node id v) (nodes c)) (components c)
    else c

updateComponentNodes :: Circuit -> ComponentID -> NodeID -> NodeID -> IO Circuit
updateComponentNodes c id pos neg =
  let searchRes = Map.lookup id (components c)
   in case searchRes of
        Just comp@(Component _ t i _ _) -> do
          c' <- createNodeIfDNE c pos
          c'' <- createNodeIfDNE c' neg
          let updated = Component id t i pos neg
           in pure (Circuit (nodes c'') (insert id updated (components c'')))
        _ -> pure c

updateComponentCurrent :: Circuit -> ComponentID -> Maybe Double -> IO Circuit
updateComponentCurrent c id i =
  let searchRes = Map.lookup id (components c)
   in case searchRes of
        Just comp@(Component _ t currI pos neg) ->
          let updated = Component id t (createCurrentVar id i) pos neg
           in pure (Circuit (nodes c) (insert id updated (components c)))
        Nothing -> pure c

updateComponentValue :: Circuit -> ComponentID -> Maybe Double -> IO Circuit
updateComponentValue c id newValue =
  let searchRes = Map.lookup id (components c)
   in case searchRes of
        Just comp@(Component (ComponentID idStr) t i pos neg) ->
          let newVar = case newValue of
                Just d -> Known d
                Nothing ->
                  Unknown
                    ( Parameter
                        ( ComponentID
                            ( ( case t of
                                  VSource _ -> getPrefix False
                                  Resistor _ -> getPrefix True
                              )
                                ++ "_"
                                ++ idStr
                            )
                        )
                    )
           in let updatedType = case componentType comp of
                    VSource _ -> VSource newVar
                    Resistor _ -> Resistor newVar
               in pure (Circuit (nodes c) (insert id (Component id updatedType i pos neg) (components c)))
        _ -> pure c

-- Remove floating nodes. If a component uses a node that doesn't exist, return Nothing.
data ValidationError
  = FloatingNode NodeID
  | DisconnectedNode NodeID
  | NegativeValue ComponentID Double
  deriving (Show, Eq)

validate :: Circuit -> Bool -> Either [ValidationError] Circuit
validate c correctFloating = do
  let freqMap = componentsToFreqMap c
      nodeMap = nodes c

      -- Find nodes that appear less than twice in freqMap (including not at all)
      badNodes = Map.foldrWithKey checkNodeConnections [] nodeMap
        where
          checkNodeConnections nid _ acc =
            let freq = Map.findWithDefault 0 nid freqMap
             in if freq < 2
                  then
                    ( if freq == 0
                        then FloatingNode nid
                        else DisconnectedNode nid
                    )
                      : acc
                  else acc

      -- Check for negative values in components
      negativeValues = Map.foldrWithKey checkComponentValues [] (components c)
        where
          checkComponentValues cid comp acc = case componentType comp of
            VSource (Known v) | v < 0 -> NegativeValue cid v : acc
            Resistor (Known r) | r < 0 -> NegativeValue cid r : acc
            _ -> acc

      -- Split errors into floating nodes and other errors
      (floatingErrors, otherErrors) = List.partition isFloatingNode (badNodes ++ negativeValues)
        where
          isFloatingNode (FloatingNode _) = True
          isFloatingNode _ = False

  -- If there are non-floating errors, always return Left
  if not (List.null otherErrors)
    then Left otherErrors
    else
      -- If correcting floating nodes, remove them and return Right
      -- Otherwise return Left with all errors
      if correctFloating
        then
          Right
            ( Circuit
                (Map.filterWithKey (\nid _ -> Map.member nid freqMap) nodeMap)
                (components c)
            )
        else Left (badNodes ++ negativeValues)

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
