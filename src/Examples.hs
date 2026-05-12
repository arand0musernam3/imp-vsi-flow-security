module Examples where

import Imp
import Types
import Parser (parseImp)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- "Initialized-to-zero" memory, occasionally handy as a starting point.
mZ :: Memory
mZ = \_ -> 0

-- Helper functions to run programs

-- Run program with specified security mode
-- showReport controls whether the security report is printed after execution.
runModeWithInput :: Bool -> ExecMode -> Program -> [Value] -> IO ()
runModeWithInput showReport mode prog@(Program lat fns p) inputs = do
    putStrLn $ "AST: " ++ show prog
    let vars = getVars p
    let bottom = head (latticeLevels lat)
    
    -- All variables start at the lattice bottom; input(ℓ, x) is the only
    -- way to inject a non-⊥ label.
    let initialLabels = \_ -> bottom
    
    -- Initial MultiMemory (empty for all levels)
    let initialMultiMemory = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
    
    let initialPC = (bottom, Set.empty)
    let initialInfluences = Map.empty

    let execute = runF 100 showReport mode lat fns vars (p, initialMultiMemory, initialLabels, [initialPC], inputs, [], [], initialInfluences)

    case mode of
        Untyped -> do
            putStrLn (boldYellow "--- Mode: UNTYPED (No Security) ---")
            execute
        Dynamic -> do
            putStrLn (boldYellow "--- Mode: DYNAMIC MONITOR ONLY ---")
            execute
        Static -> do
            putStrLn (boldYellow "--- Mode: STATIC TYPING ONLY ---")
            let staticEnv = initEnv lat
            case cmdType lat fns vars staticEnv bottom p of
                WellTyped _ _ -> do
                    putStrLn (boldGreen "--- Static Analysis: WELL-TYPED ---")
                    execute
                TypeError msg -> do
                    putStrLn (boldRed "--- Static Analysis: TYPE ERROR ---")
                    putStrLn msg
                    putStrLn (dim "--- Execution Halting due to static type error ---")
        Both -> do
            putStrLn (boldYellow "--- Mode: BOTH STATIC AND DYNAMIC ---")
            let staticEnv = initEnv lat
            case cmdType lat fns vars staticEnv bottom p of
                WellTyped _ _ -> do
                    putStrLn (boldGreen "--- Static Analysis: WELL-TYPED ---")
                    execute
                TypeError msg -> do
                    putStrLn (boldRed "--- Static Analysis: TYPE ERROR ---")
                    putStrLn msg
                    putStrLn (dim "--- Execution Halting due to static type error ---")

runStringModeWithInput :: Bool -> ExecMode -> String -> [Value] -> IO ()
runStringModeWithInput showReport mode s inputs = case parseImp s of
    Left err -> print err
    Right p  -> runModeWithInput showReport mode p inputs

-- Backwards compatibility and convenience
runTyped :: Program -> IO ()
runTyped p = runModeWithInput True Static p []

runTypedWithInput :: Program -> [Value] -> IO ()
runTypedWithInput p i = runModeWithInput True Static p i

runUntyped :: Program -> IO ()
runUntyped p = runModeWithInput True Untyped p []

runUntypedWithInput :: Program -> [Value] -> IO ()
runUntypedWithInput p i = runModeWithInput True Untyped p i

runStringTyped :: String -> IO ()
runStringTyped s = runStringModeWithInput True Static s []

runStringTypedWithInput :: String -> [Value] -> IO ()
runStringTypedWithInput s inputs = runStringModeWithInput True Static s inputs

runStringUntyped :: String -> IO ()
runStringUntyped s = runStringModeWithInput True Untyped s []

runStringUntypedWithInput :: String -> [Value] -> IO ()
runStringUntypedWithInput s inputs = runStringModeWithInput True Untyped s inputs
