module Examples where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Imp
import Parser (parseImp)

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
  let initialMultiMemory = Map.fromList [(lId l, \_ -> 0) | l <- latticeLevels lat]

  let initialPC = (bottom, Set.empty)
  let initialInfluences = Map.empty

  let execute = runF 100 showReport mode lat fns vars (p, initialMultiMemory, initialLabels, [initialPC], inputs, [], [], initialInfluences)

  case mode of
    Untyped -> do
      putStrLn (boldYellow "--- Mode: UNTYPED (No Security) ---")
      execute
    DynamicNSU -> do
      putStrLn (boldYellow "--- Mode: DYNAMIC MONITOR (NSU) ---")
      execute

runStringModeWithInput :: Bool -> ExecMode -> String -> [Value] -> IO ()
runStringModeWithInput showReport mode s inputs = case parseImp s of
  Left err -> print err
  Right p -> runModeWithInput showReport mode p inputs
