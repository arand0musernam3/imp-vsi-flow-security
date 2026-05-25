module Examples where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Imp
import Parser (parseImp)

color :: String -> String -> String
color code s = code ++ s ++ "\x1b[0m"

bold, dim :: String -> String
bold = color "\x1b[1m"
dim = color "\x1b[2m"

boldRed, boldGreen, boldCyan, boldYellow :: String -> String
boldRed = color "\x1b[1;31m"
boldGreen = color "\x1b[1;32m"
boldCyan = color "\x1b[1;36m"
boldYellow = color "\x1b[1;33m"

-- Resolve a channel name to its Level in the given lattice. Errors out
-- with a clear message if the name is not present (rather than failing
-- silently inside the monitor).
resolveChannel :: SecurityLattice -> String -> Level
resolveChannel lat name =
  case filter (\l -> lName l == name) (latticeLevels lat) of
    (l : _) -> l
    [] ->
      error $
        "Input tape error: channel "
          ++ name
          ++ " is not a level in the lattice "
          ++ show (map lName (latticeLevels lat))
          ++ "."

resolveInputs :: SecurityLattice -> [(String, Value)] -> [(Level, Value)]
resolveInputs lat = map (\(ch, v) -> (resolveChannel lat ch, v))

runModeWithInput :: Bool -> ExecMode -> Program -> [(String, Value)] -> IO ()
runModeWithInput showReport mode prog@(Program lat fns p) inputs = do
  putStrLn $ "AST: " ++ show prog
  let vars    = getVars p
      cfg     = initialConfig lat p (resolveInputs lat inputs)
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

runStringModeWithInput :: Bool -> ExecMode -> String -> [(String, Value)] -> IO ()
runStringModeWithInput showReport mode s inputs = case parseImp s of
  Left err -> print err
  Right p -> runModeWithInput showReport mode p inputs

printSecurityReport :: SecurityLattice -> [VarName] -> MultiMemory -> Labels -> [(Level, Value)] -> Influences -> Partials -> IO ()
printSecurityReport lat vars mm labs outputs infl partials = do
  let levels = latticeLevels lat
      obsW = maximum (1 : map (length . show) levels)
      nameW = maximum (4 : map length vars)
      labW = maximum (5 : map (length . show . labs) vars)
      valW =
        maximum
          ( 6
              : map (length . show) levels
              ++ [length (show (getMem mm l x)) | x <- vars, l <- levels]
          )
  putStrLn $ boldCyan "  -- Security Report ----------------------------------------"
  putStrLn $ bold "  Outputs (by observer)"
  putStrLn $ "    " ++ padR (9 + obsW) "emitted" ++ " : " ++ show outputs
  mapM_
    ( \l ->
        let visible = [v | (ch, v) <- outputs, ch <= l]
         in putStrLn $ "    observer " ++ padR obsW (show l) ++ " : " ++ show visible
    )
    levels
  putStrLn ""
  putStrLn $ bold "  Variable Visibility (final state)"
  let hdr =
        dim $
          "    "
            ++ padR nameW "name"
            ++ "  "
            ++ padR labW "label"
            ++ concatMap (\l -> padL (valW + 1) (show l)) levels
  putStrLn hdr
  mapM_
    ( \x ->
        let row =
              "    "
                ++ padR nameW x
                ++ "  "
                ++ padR labW (show (labs x))
                ++ concatMap (\l -> padL (valW + 1) (show (getMem mm l x))) levels
         in putStrLn row
    )
    vars
  putStrLn ""
  putStrLn $ bold "  Influences"
  mapM_
    ( \x ->
        let deps = Map.findWithDefault Set.empty x infl
            depsStr =
              if Set.null deps
                then "\8709"
                else "{ " ++ intercalate ", " (Set.toList deps) ++ " }"
         in putStrLn $ "    " ++ x ++ "  \8592  " ++ depsStr
    )
    vars
  putStrLn ""
  putStrLn $ bold "  Partials (PU-marked)"
  let pStr =
        if Set.null partials
          then "\8709"
          else "{ " ++ intercalate ", " (Set.toList partials) ++ " }"
  putStrLn $ "    " ++ pStr
  putStrLn ""
  where
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    padL n s = replicate (max 0 (n - length s)) ' ' ++ s

runF :: Integer -> Bool -> ExecMode -> SecurityLattice -> [Function] -> [VarName] -> Configuration -> IO ()
runF n showReport mode lat fns vars cfg =
  case evalF n mode lat fns cfg of
    OutOfFuel -> print "OutOfFuel"
    Finished mm' labs' o' infl' p' -> do
      putStrLn $ bold "Output: " ++ show (map snd o')
      when showReport $ printSecurityReport lat vars mm' labs' o' infl' p'
