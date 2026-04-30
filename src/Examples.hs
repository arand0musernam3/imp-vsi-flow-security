module Examples where

import Imp
import Types
import Parser (parseImp)

-- EXAMPLES
-- Memory environments with different secrets

-- Example "initialized-to-zero" memory
mZ :: Memory
mZ = \_ -> 0

m0 :: Memory
m0 = update mZ "y_s" 0

m1 :: Memory
m1 = update mZ "y_s" 1

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
run100 :: [Function] -> Cmd -> Environment -> Configuration -> IO ()
run100 fns p env config = runF 100 fns (getVars p) env config

-- Basic run (empty input)
runTyped :: Program -> Memory -> IO ()
runTyped p m = runTypedWithInput p m []

-- Run with explicit input tape
runTypedWithInput :: Program -> Memory -> [Value] -> IO ()
runTypedWithInput prog@(Program lat fns p) m inputs = 
  do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let env  = initEnv lat vars
    -- PC starts at the bottom level of the lattice
    let bottom = head (latticeLevels lat)
    case cmdType lat fns vars env bottom p of 
      WellTyped env' -> run100 fns p env' (p, m, inputs, [], [])
      TypeError msg -> putStrLn msg

runUntyped :: Program -> Memory -> IO ()
runUntyped p m = runUntypedWithInput p m []

runUntypedWithInput :: Program -> Memory -> [Value] -> IO ()
runUntypedWithInput prog@(Program lat fns p) m inputs =
  do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let env  = initEnv lat vars
    run100 fns p env (p, m, inputs, [], [])

runStringTyped :: String -> Memory -> IO ()
runStringTyped s m = runStringTypedWithInput s m []

runStringTypedWithInput :: String -> Memory -> [Value] -> IO ()
runStringTypedWithInput s m inputs = case parseImp s of
    Left err -> print err
    Right p  -> runTypedWithInput p m inputs

runStringUntyped :: String -> Memory -> IO ()
runStringUntyped s m = case parseImp s of
    Left err -> print err
    Right p  -> runUntyped p m

runStringUntypedWithInput :: String -> Memory -> [Value] -> IO ()
runStringUntypedWithInput s m inputs = case parseImp s of
    Left err -> print err
    Right p  -> runUntypedWithInput p m inputs
