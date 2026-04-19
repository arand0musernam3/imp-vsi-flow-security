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
pdfExample1 = "input(secret, x); y := 0; if x then output(public, y) else output(public, y)"

-- PDF Example 2 (Rejected)
pdfExample2 :: String
pdfExample2 = "input(secret, x); y := 0; if x then output(public, y) else skip"

---
-- Helper functions to run programs

-- Run program for at most 100 steps
run100 :: Cmd -> Environment -> Configuration -> IO ()
run100 p env config = runF 100 (getVars p) env config

-- Basic run (empty input)
runTyped :: Cmd -> Memory -> IO ()
runTyped p m = runTypedWithInput p m []

-- Run with explicit input tape
runTypedWithInput :: Cmd -> Memory -> [Value] -> IO ()
runTypedWithInput p m inputs = 
  let vars = getVars p
      env  = initEnv vars
  in case cmdType vars env public p of 
    WellTyped env' -> run100 p env' (p, m, inputs, [])
    TypeError msg -> print msg

runUntyped :: Cmd -> Memory -> IO ()
runUntyped p m = 
  let vars = getVars p
      env  = initEnv vars
  in run100 p env (p, m, [], [])

runUntypedWithInput :: Cmd -> Memory -> [Value] -> IO ()
runUntypedWithInput p m inputs =
  let vars = getVars p
      env  = initEnv vars
  in run100 p env (p, m, inputs, [])

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