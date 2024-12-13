# Circuit Solver
## Members: Kashyap Chaturvedula (Kashyap456), Brady Zhou (bradyz314) 

Circuit Solver is a small shell application that allows users to build simple electric circuits and solve for unknown variables.

## Module Organization

### App
This folder only contains Main.hs, where the shell program lives.

### Src
This folder contains all the source files:
  - **Circuit.hs** defines all the components of a circuit. This includes nodes, components, and circuits.
  - **CircuitGraph.hs** defines several graph algorithms for traversing a circuit, which is a cyclic graph.
  - **CircuitModifiers.hs** defines several functions that allow the user to modify an existing circuit.
  - **Path.hs** is a helper module that defines a path representation of traversed nodes and components.
  - **Solver.hs** defines the functionality for constructing equations from circuits and solving them.
  - **YamlParser.hs** defines the parsers for parsing YAML files.
  - **CircuitSaver.hs** defines functions for loading and saving a circuit from/to a YAML file (uses YamlParser.hs)

### Test
This folder contains all of the test files:
  - **TestCommons.hs** defines some common functions used within the other test files.
  - **CircuitGraphTest.hs** tests *CircuitGraph.hs* and *Path.hs*. It has unit test cases for path operations.  
  - **CircuitTest.hs** defines an arbitrary instance for circuits and several QuickCheck properties. For example, we have properties that test roundtrip properties and check valid circuit structure.
  - **E2ETests.hs** is a simple unit test case that checks the entire process of loading a circuit from a file, constructing its equations, and solving them.
  - **SolverTest.hs** defines unit test cases for solving different types of equations, such as Ohm's law equations or KCL/KVL equations.
  - **YamlParserTest.hs** defines two unit test cases for parsing sample files. It also defines an arbitrary instance for YAML and a roundtrip QuickCheck property.

## Building, Running, and Testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

Additional dependencies used were parsec, hmatrix, and text. It is very difficult to set up hmatrix on a Windows machine!

