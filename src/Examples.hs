module Examples where

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
  let vars    = getVars p
      cfg     = initialConfig lat p inputs
      execute = runF 100 showReport mode lat fns vars cfg

  case mode of
    Untyped -> do
      putStrLn (boldYellow "--- Mode: UNTYPED (No Security) ---")
      execute
    DynamicNSU -> do
      putStrLn (boldYellow "--- Mode: DYNAMIC MONITOR (NSU) ---")
      execute
    DynamicPU -> do
      putStrLn (boldYellow "--- Mode: DYNAMIC MONITOR (PU) ---")
      execute

runStringModeWithInput :: Bool -> ExecMode -> String -> [Value] -> IO ()
runStringModeWithInput showReport mode s inputs = case parseImp s of
  Left err -> print err
  Right p -> runModeWithInput showReport mode p inputs
