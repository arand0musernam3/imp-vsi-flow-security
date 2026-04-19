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
-- (Now that we have a parser)

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

---
-- Helper functions to run programs

varsOfInterest :: [VarName]
varsOfInterest = ["x_p", "y_s"] 

-- Run program for at most 100 steps
run100 :: Configuration -> IO ()
run100 = runF 100 varsOfInterest -- obs: fuel argument of 100 steps hardcoded

checkWithVarsOfInterest :: Cmd -> TypeRes
checkWithVarsOfInterest = cmdType (initEnv varsOfInterest) Public

runTyped :: Cmd -> Memory -> IO ()
runTyped p m = do 
  case checkWithVarsOfInterest p of 
    WellTyped -> run100 (p, m)
    TypeError msg -> print msg

runUntyped :: Cmd -> Memory -> IO ()
runUntyped p m = run100 (p, m)

-- Parse a program from a string and run it (typed or untyped)

runStringTyped :: String -> Memory -> IO ()
runStringTyped s m = case parseImp s of
    Left err -> print err
    Right p  -> runTyped p m

runStringUntyped :: String -> Memory -> IO ()
runStringUntyped s m = case parseImp s of
    Left err -> print err
    Right p  -> runUntyped p m
