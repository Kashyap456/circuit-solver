{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Circuit
import CircuitModifiers (addComponent, addNode, updateNodeVoltage, validate, updateComponentCurrent, updateComponentValue, updateComponentNodes)
import CircuitSaver (parseCircuit, saveCircuit)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import System.IO.Error qualified as IO
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
          let result = validate state
           in do
                putStrLn ("Circuit is " ++ (if isNothing result then "invalid" else "valid"))
                case result of
                  Just c -> case args of
                    x : xs -> go (if x == "-r" then c else state)
                    _ -> go state
                  _ -> go state
        Just ("addNode", args) -> do
          c <- case args of
            [] -> addNode state Nothing Nothing
            [id] -> addNode state (Just (NodeID id)) Nothing
            id : voltage : _ -> addNode state (Just (NodeID id)) (readMaybe voltage)
          go c
        Just ("addComponent", t : pos : neg : args) ->
          let isResistor = t == "R"
           in do
                c <- case args of
                  [] -> addComponent state (NodeID pos) (NodeID neg) Nothing isResistor Nothing
                  [i] -> addComponent state (NodeID pos) (NodeID neg) (readMaybe i) isResistor Nothing
                  i : v : _ -> addComponent state (NodeID pos) (NodeID neg) (readMaybe i) isResistor (readMaybe v)
                go c
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
        Just ("help", _) -> do
          putStrLn "load <filename>: Load a circuit based on the specified file"
          putStrLn "save <filename>: Save the current circuit to the specified file"
          putStrLn "validate [-r]: Check if the current circuit is valid. Use -r to remove floating nodes"
          putStrLn "addNode [id] [voltage]: Add a node to the current circuit"
          putStrLn "addComponent <R|V> <posID> <negID> [current] [value]: Add a component to the current circuit"
          putStrLn "setNodeVoltage <id> [value]: Set a node's voltage"
          putStrLn "setCurrent <id> [value]: Set a component's current"
          putStrLn "setValue <id> [value]: Set a component's specific value"
          putStrLn "setNodes <id> <posID> <negID>: Set a component's positive and negative nodes"
          go state
        _ -> do
          putStrLn "Invalid command."
          go state
