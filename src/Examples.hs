module Examples where

import Imp
import Types
import Parser (parseImp)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

-- Run program with specified security mode
runModeWithInput :: ExecMode -> Program -> [Value] -> IO ()
runModeWithInput mode prog@(Program lat fns p) inputs = do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let bottom = head (latticeLevels lat)
    
    -- Initial Dynamic Labels (using naming convention)
    let initialLabels = levelFromName lat
    
    -- Initial MultiMemory (empty for all levels)
    let initialMultiMemory = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
    
    let initialPC = (bottom, Set.empty)
    let initialInfluences = Map.empty
    
    let execute = runF 100 mode lat fns vars (p, initialMultiMemory, initialLabels, [initialPC], inputs, [], [], initialInfluences)

    case mode of
        Untyped -> do
            putStrLn "--- Mode: UNTYPED (No Security) ---"
            execute
        Dynamic -> do
            putStrLn "--- Mode: DYNAMIC MONITOR ONLY ---"
            execute
        Static -> do
            putStrLn "--- Mode: STATIC TYPING ONLY ---"
            let staticEnv = initEnv lat vars
            case cmdType lat fns vars staticEnv bottom p of 
                WellTyped _ _ -> do
                    putStrLn "--- Static Analysis: WELL-TYPED ---"
                    execute
                TypeError msg -> do
                    putStrLn $ "--- Static Analysis: TYPE ERROR ---"
                    putStrLn msg
                    putStrLn "--- Execution Halting due to static type error ---"
        Both -> do
            putStrLn "--- Mode: BOTH STATIC AND DYNAMIC ---"
            let staticEnv = initEnv lat vars
            case cmdType lat fns vars staticEnv bottom p of 
                WellTyped _ _ -> do
                    putStrLn "--- Static Analysis: WELL-TYPED ---"
                    execute
                TypeError msg -> do
                    putStrLn $ "--- Static Analysis: TYPE ERROR ---"
                    putStrLn msg
                    putStrLn "--- Execution Halting due to static type error ---"

runStringModeWithInput :: ExecMode -> String -> [Value] -> IO ()
runStringModeWithInput mode s inputs = case parseImp s of
    Left err -> print err
    Right p  -> runModeWithInput mode p inputs

-- Backwards compatibility and convenience
runTyped :: Program -> IO ()
runTyped p = runModeWithInput Static p []

runTypedWithInput :: Program -> [Value] -> IO ()
runTypedWithInput p i = runModeWithInput Static p i

runUntyped :: Program -> IO ()
runUntyped p = runModeWithInput Untyped p []

runUntypedWithInput :: Program -> [Value] -> IO ()
runUntypedWithInput p i = runModeWithInput Untyped p i

runStringTyped :: String -> IO ()
runStringTyped s = runStringModeWithInput Static s []

runStringTypedWithInput :: String -> [Value] -> IO ()
runStringTypedWithInput s inputs = runStringModeWithInput Static s inputs

runStringUntyped :: String -> IO ()
runStringUntyped s = runStringModeWithInput Untyped s []

runStringUntypedWithInput :: String -> [Value] -> IO ()
runStringUntypedWithInput s inputs = runStringModeWithInput Untyped s inputs
