module Examples where

import Imp
import Types
import Parser (parseImp)
import qualified Data.Map as Map

-- EXAMPLES
-- Memory environments with different secrets

-- Example "initialized-to-zero" memory
mZ :: Memory
mZ = \_ -> 0

-- Sample programs in strings

-- example1: x_p := y_s
example1 :: String
example1 = "x_p := y_s"

-- example3: y_s := 42; x_p := y_s
example3 :: String
example3 = "y_s := 42; x_p := y_s"

-- example4: if y_s then x_p := 1 else x_p := 0
example4 :: String
example4 = "if y_s then x_p := 1 else x_p := 0"

-- exercise2: while y_s do skip
exercise2 :: String
exercise2 = "while y_s do skip"

-- PDF Example 1 (Rejected)
pdfExample1 :: String
pdfExample1 = "input(high, x); y := 0; if x then output(low, y) else output(low, y)"

-- PDF Example 2 (Rejected)
pdfExample2 :: String
pdfExample2 = "input(high, x); y := 0; if x then output(low, y) else skip"

---
-- Helper functions to run programs

-- Run program for at most 100 steps
run100 :: SecurityLattice -> [Function] -> Cmd -> MultiMemory -> Labels -> IO ()
run100 lat fns p mm labs = runF 100 lat fns (getVars p) (p, mm, labs, [head (latticeLevels lat)], [], [], [])

-- Basic run (empty input)
runTyped :: Program -> IO ()
runTyped p = runTypedWithInput p []

-- Run with explicit input tape
runTypedWithInput :: Program -> [Value] -> IO ()
runTypedWithInput prog@(Program lat fns p) inputs = 
  do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let bottom = head (latticeLevels lat)
    
    -- Initial Dynamic Labels (everything is bottom)
    let initialLabels _ = bottom
    
    -- Initial MultiMemory (empty for all levels)
    let initialMultiMemory = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
    
    -- Static analysis check (using Types.hs)
    let staticEnv = initEnv lat vars
    case cmdType lat fns vars staticEnv bottom p of 
      WellTyped _ -> do
          putStrLn "--- Static Analysis: WELL-TYPED ---"
          runF 100 lat fns vars (p, initialMultiMemory, initialLabels, [bottom], inputs, [], [])
      TypeError msg -> do
          putStrLn $ "--- Static Analysis: TYPE ERROR ---"
          putStrLn msg
          putStrLn "--- Proceeding with Runtime Execution (Untyped) ---"
          runF 100 lat fns vars (p, initialMultiMemory, initialLabels, [bottom], inputs, [], [])

runUntyped :: Program -> IO ()
runUntyped p = runUntypedWithInput p []

runUntypedWithInput :: Program -> [Value] -> IO ()
runUntypedWithInput prog@(Program lat fns p) inputs =
  do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let bottom = head (latticeLevels lat)
    let initialLabels _ = bottom
    let initialMultiMemory = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
    runF 100 lat fns vars (p, initialMultiMemory, initialLabels, [bottom], inputs, [], [])

runStringTyped :: String -> IO ()
runStringTyped s = runStringTypedWithInput s []

runStringTypedWithInput :: String -> [Value] -> IO ()
runStringTypedWithInput s inputs = case parseImp s of
    Left err -> print err
    Right p  -> runTypedWithInput p inputs

runStringUntyped :: String -> IO ()
runStringUntyped s = case parseImp s of
    Left err -> print err
    Right p  -> runUntyped p

runStringUntypedWithInput :: String -> [Value] -> IO ()
runStringUntypedWithInput s inputs = case parseImp s of
    Left err -> print err
    Right p  -> runUntypedWithInput p inputs
