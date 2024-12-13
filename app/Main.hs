{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Circuit
import CircuitModifiers (addComponent, addNode, deleteComponent, deleteNode, updateComponentCurrent, updateComponentNodes, updateComponentValue, updateNodeVoltage, validate)
import CircuitSaver (parseCircuit, saveCircuit)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Solver (solve)
import System.IO.Error qualified as IO
import TestCommons (printSolution)
import Text.Read (readMaybe)

initialCircuit :: Circuit
initialCircuit = Circuit Map.empty Map.empty

main :: IO ()
main = go initialCircuit
  where
    go :: Circuit -> IO ()
    go state = do
      putStr "$ "
      input <- getLine
      case List.uncons (words input) of
        Just ("load", [filename]) -> do
          result <- parseCircuit filename
          case result of
            Just c -> go c
            _ -> do
              putStrLn "Encountered error loading file."
              go state
        Just ("save", [filename]) -> do
          IO.catchIOError
            (saveCircuit filename state)
            (\e -> putStrLn ("Error: " ++ show e))
          go state
        Just ("validate", args) -> do
          let result = validate state (case args of x : _ -> x == "-r"; _ -> False)
           in do
                case result of
                  Left errs -> do
                    putStrLn $ "Errors: " ++ show errs
                    go state
                  Right c -> go c
        Just ("addNode", args) -> do
          c <- case args of
            [] -> addNode state Nothing Nothing
            [id] -> addNode state (Just (NodeID id)) Nothing
            id : voltage : _ -> addNode state (Just (NodeID id)) (readMaybe voltage)
          go c
        Just ("deleteNode", id : _) -> go (deleteNode state (NodeID id))
        Just ("addComponent", t : pos : neg : args) ->
          let isResistor = t == "R"
           in do
                c <- case args of
                  [] -> addComponent state (NodeID pos) (NodeID neg) Nothing isResistor Nothing
                  [i] -> addComponent state (NodeID pos) (NodeID neg) (readMaybe i) isResistor Nothing
                  i : v : _ -> addComponent state (NodeID pos) (NodeID neg) (readMaybe i) isResistor (readMaybe v)
                go c
        Just ("deleteComponent", id : _) -> go (deleteComponent state (ComponentID id))
        Just ("setNodeVoltage", id : args) ->
          let c = case args of
                [] -> updateNodeVoltage state (NodeID id) Nothing
                (v : _) -> updateNodeVoltage state (NodeID id) (readMaybe v)
           in go c
        Just ("setCurrent", id : args) -> do
          c <- case args of
            [] -> updateComponentCurrent state (ComponentID id) Nothing
            (v : _) -> updateComponentCurrent state (ComponentID id) (readMaybe v)
          go c
        Just ("setValue", id : args) -> do
          c <- case args of
            [] -> updateComponentValue state (ComponentID id) Nothing
            (v : _) -> updateComponentValue state (ComponentID id) (readMaybe v)
          go c
        Just ("setNodes", id : pos : neg : _) -> do
          c <- updateComponentNodes state (ComponentID id) (NodeID pos) (NodeID neg)
          go c
        Just ("printCircuit", _) -> do
          print state
          go state
        Just ("solve", _) -> do
          putStrLn (printSolution (solve state))
          go state
        Just ("help", _) -> do
          putStrLn "load <filename>: Load a circuit based on the specified file"
          putStrLn "save <filename>: Save the current circuit to the specified file"
          putStrLn "printCircuit: Print the current circuit"
          putStrLn "validate [-r]: Check if the current circuit is valid. Use -r to remove floating nodes"
          putStrLn "addNode [id] [voltage]: Add a node to the current circuit. Voltage becomes unknown if not specified"
          putStrLn "deleteNode <id>: Delete a node from the current circuit"
          putStrLn "addComponent <R|V> <posID> <negID> [current] [value]: Add a component to the current circuit"
          putStrLn "deleteComponent <id>: Delete a component from the current circuit"
          putStrLn "setNodeVoltage <id> [value]: Set a node's voltage"
          putStrLn "setCurrent <id> [value]: Set a component's current"
          putStrLn "setValue <id> [value]: Set a component's specific value"
          putStrLn "setNodes <id> <posID> <negID>: Set a component's positive and negative nodes"
          putStrLn "solve: Attempt to solve the current circuit"
          go state
        _ -> do
          putStrLn "Invalid command."
          go state
