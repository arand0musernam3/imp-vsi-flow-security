module Examples where

import Imp
import Types

-- EXAMPLES
-- Memory environments with different secrets

-- Example "initialized-to-zero" memory
mZ :: Memory
mZ = \_ -> 0

m0 :: Memory
m0 = update mZ "y_s" 0

m1 :: Memory
m1 = update mZ "y_s" 1

-- Sample programs in AST
-- (Because we do not have a parser yet)

-- example1: x_p := y_s
example1 :: Cmd
example1 = Assign "x_p" (VarExpr "y_s")

-- example3: y_s := 42; x_p := y_s
example3 :: Cmd
example3 = Seq (Assign "y_s" (IntExpr 42))
               (Assign "x_p" (VarExpr "y_s"))

-- example4: if y_s then x_p := 1 else x_p := 0
example4 :: Cmd
example4 = If (VarExpr "y_s")               
                (Assign "x_p" (IntExpr 1))  
                (Assign "x_p" (IntExpr 0))  

-- exercise2: while y_s do skip
exercise2 :: Cmd
exercise2 = While (VarExpr "y_s") Skip

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
