module CircuitTest where

import Circuit
import Data.List qualified as List
import Data.Map qualified as Map
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    choose,
    chooseInt,
    elements,
    frequency,
    listOf1,
    oneof,
    vectorOf,
    (.&&.),
  )

genNodeID :: Gen String
genNodeID = do
  prefix <- elements ["n", "node"]
  num <- chooseInt (1, 20)
  return $ prefix ++ show num

genComponentID :: Gen String
genComponentID = do
  prefix <- elements ["v", "r", "vs", "res"]
  num <- chooseInt (1, 15)
  return $ prefix ++ show num

genValue :: Gen Var
genValue =
  oneof
    [ Known <$> choose (-24, 24),
      Unknown . Parameter <$> do
        prefix <- elements ["p", "param"]
        num <- chooseInt (1, 10)
        return $ prefix ++ show num
    ]

genComponent :: Gen Component
genComponent = do
  cid <- genComponentID
  ctype <-
    frequency
      [ (1, VSource <$> genValue),
        (3, Resistor <$> genValue)
      ]
  curr <- genValue
  pos <- genNodeID
  Component cid ctype curr pos <$> genNodeID

genNode :: Gen Node
genNode = do
  nid <- genNodeID
  voltage <-
    oneof
      [ Known <$> choose (-12, 12),
        Unknown . NodeVoltage <$> genNodeID
      ]
  return $ Node nid voltage

-- Generator for Circuits
instance Arbitrary Circuit where
  arbitrary = do
    loopSize <- choose (3, 5)
    loopNodes <- vectorOf loopSize $ do
      nid <- genNodeID
      return $ Node nid (Unknown . NodeVoltage $ nid)

    -- Add one node with known voltage
    knownNode <- do
      nid <- genNodeID
      return $ Node nid (Known 5.0)

    let loopNodes = knownNode : loopNodes

    -- Create components for the loop
    let loopNodeIDs = map nodeID loopNodes
    loopComponents <-
      sequence
        [ do
            cid <- genComponentID
            ctype <-
              frequency
                [ (1, VSource <$> genValue),
                  (3, Resistor . Known <$> choose (1, 1000))
                ]
            let curr = Unknown . Parameter $ "i_" ++ cid
            return $
              Component
                cid
                ctype
                curr
                (loopNodeIDs !! i)
                (loopNodeIDs !! ((i + 1) `mod` loopSize))
          | i <- [0 .. loopSize - 1]
        ]

    -- Add some additional nodes and components which cause parallel paths
    extraNodeCount <- choose (0, 2)
    extraNodes <- vectorOf extraNodeCount $ do
      nid <- genNodeID
      return $ Node nid (Unknown . NodeVoltage $ nid)

    let allNodes = loopNodes ++ extraNodes
        allNodeIDs = map nodeID allNodes
        loopNodeIDSet = map nodeID loopNodes

    -- Add connections, ensuring at least one end is from the main loop
    extraCompCount <- choose (0, 2 * extraNodeCount) -- Scale with extra nodes
    extraComponents <- vectorOf extraCompCount $ do
      cid <- genComponentID
      ctype <-
        frequency
          [ (1, VSource <$> genValue),
            (3, Resistor . Known <$> choose (1, 1000))
          ]
      let curr = Unknown . Parameter $ "i_" ++ cid
      -- Ensure at least one end is from the main loop
      pos <- elements loopNodeIDSet -- First end must be from loop
      neg <- elements $ filter (/= pos) allNodeIDs -- Second can be from any node
      return $ Component cid ctype curr pos neg

    return $
      Circuit
        (Map.fromList [(nodeID n, n) | n <- allNodes])
        (loopComponents ++ extraComponents)

-- Properties

-- Every circuit should have at least one node and component
prop_nonEmpty :: Circuit -> Bool
prop_nonEmpty (Circuit ns cs) =
  not (Map.null ns) && not (null cs)

-- Node IDs in components should exist in nodes map
prop_nodeExists :: Circuit -> Bool
prop_nodeExists c@(Circuit ns cs) =
  case validate c of
    Nothing -> True
    Just _ ->
      all
        ( \comp ->
            Map.member (nodePos comp) ns
              && Map.member (nodeNeg comp) ns
        )
        cs

-- A validated circuit should maintain its structure
prop_validatePreservesStructure :: Circuit -> Property
prop_validatePreservesStructure c =
  case validate c of
    Nothing -> property True
    Just c' ->
      nodes c'
        `Map.isSubmapOf` nodes c
        .&&. components c'
        `List.isSubsequenceOf` components c