module TestUtils
  ( Expectation (..),
    ModeOutcome (..),
    runCapture,
    runTest,
    runObserverTest,
    staticTest,
    compareModes,
    bold,
    dim,
    boldGreen,
    boldRed,
    boldCyan,
    boldYellow,
  )
where

import Control.Exception (SomeException, try)
import Data.List (isPrefixOf)
import Examples
  ( bold,
    boldCyan,
    boldGreen,
    boldRed,
    boldYellow,
    dim,
    printSecurityReport,
    runStringModeWithInput,
  )
import Imp
import Parser (parseImp)
import Types (TypeRes (..), cmdType, initEnv)

data Expectation = ShouldPass | ShouldFail
  deriving (Eq, Show)

data ModeOutcome
  = Accept String      -- successfully completed
  | Reject String      -- static type-checker rejected the program
  | Abort String       -- dynamic monitor aborted
  | Stuck String       -- other failure
  deriving (Show)

outcomeKind :: ModeOutcome -> String
outcomeKind (Accept _) = "ACCEPT"
outcomeKind (Reject _) = "REJECT"
outcomeKind (Abort _) = "ABORT"
outcomeKind (Stuck _) = "STUCK"

outcomeDetail :: ModeOutcome -> String
outcomeDetail (Accept d) = d
outcomeDetail (Reject d) = d
outcomeDetail (Abort d) = d
outcomeDetail (Stuck d) = d

sameKind :: ModeOutcome -> ModeOutcome -> Bool
sameKind a b = outcomeKind a == outcomeKind b

-- Run a program silently
runCapture ::
  ExecMode ->
  String ->
  [Value] ->
  IO (MultiMemory, Labels, [(Level, Value)], Influences, Partials, SecurityLattice, [VarName])
runCapture mode prog inputs = case parseImp prog of
  Left err -> error (show err)
  Right (Program lat fns mainCmd) ->
    let vars = getVars mainCmd
        cfg = initialConfig lat mainCmd inputs
     in case evalF 1000 mode lat fns cfg of
          Finished mm labs o infl p -> return (mm, labs, o, infl, p, lat, vars)
          OutOfFuel -> error "OutOfFuel"

-- Static type-check only
staticTest :: String -> String -> Expectation -> IO Bool
staticTest name prog expected = do
  putStrLn ""
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ bold "TEST: " ++ name
  putStrLn (dim (replicate 60 '-'))
  putStrLn "  mode:    Static (type-check only)"
  putStrLn $ "  expect:  " ++ show expected
  putStrLn $ "  program: " ++ prog
  let result = case parseImp prog of
        Left err -> Left ("parse error: " ++ show err)
        Right (Program lat fns mainCmd) ->
          let vars = getVars mainCmd
              env = initEnv lat
              bot = head (latticeLevels lat)
           in case cmdType lat fns vars env bot mainCmd of
                WellTyped _ _ -> Right ()
                TypeError msg -> Left msg
  let (passed, msg) = case (result, expected) of
        (Left e, ShouldFail) -> (True, "rejected: " ++ shortenS e)
        (Right _, ShouldPass) -> (True, "well-typed")
        (Left e, ShouldPass) -> (False, "unexpected rejection: " ++ shortenS e)
        (Right _, ShouldFail) -> (False, "expected rejection but well-typed")
  putStrLn $ "  >>> " ++ (if passed then boldGreen "PASS" else boldRed "FAIL") ++ " - " ++ dim msg
  return passed
  where
    shortenS s = if length s > 250 then take 250 s ++ "..." else s

-- Run the program and compare what an observer at, observerName sees
runObserverTest ::
  String ->
  ExecMode ->
  String ->
  [Value] ->
  String ->
  [Value] ->
  Bool ->
  IO Bool
runObserverTest name mode prog inputs observerName expected showReport = do
  putStrLn ""
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ bold "TEST: " ++ name
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ "  mode:      " ++ show mode
  putStrLn $ "  inputs:    " ++ show inputs
  putStrLn $ "  observer:  " ++ observerName ++ "   (filter: ch ⊑ observer)"
  putStrLn $ "  expect:    visible = " ++ show expected
  putStrLn $ "  program:   " ++ prog
  result <-
    try (runCapture mode prog inputs) ::
      IO
        ( Either
            SomeException
            (MultiMemory, Labels, [(Level, Value)], Influences, Partials, SecurityLattice, [VarName])
        )
  let (passed, msg) = case result of
        Right (_, _, o, _, _, lat, _) ->
          case filter ((== observerName) . lName) (latticeLevels lat) of
            [] -> (False, "level " ++ observerName ++ " not in lattice")
            (obs : _) ->
              let visible = [v | (ch, v) <- o, ch <= obs]
               in if visible == expected
                    then (True, "got " ++ show visible)
                    else (False, "expected " ++ show expected ++ ", got " ++ show visible)
        Left e -> (False, "unexpected error: " ++ shorten (show e))
  putStrLn $ "  >>> " ++ (if passed then boldGreen "PASS" else boldRed "FAIL") ++ " - " ++ dim msg
  if showReport
    then case result of
      Right (mm, labs, o, infl, p, lat, vars) ->
        printSecurityReport lat vars mm labs o infl p
      Left _ ->
        putStrLn (dim "  (no security report — execution terminated with error)")
    else return ()
  return passed
  where
    shorten s = if length s > 250 then take 250 s ++ "..." else s

runTest :: String -> ExecMode -> String -> [Value] -> Expectation -> Bool -> IO Bool
runTest name mode prog inputs expected showReport = do
  putStrLn ""
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ bold "TEST: " ++ name
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ "  mode:    " ++ show mode
  putStrLn $ "  inputs:  " ++ show inputs
  putStrLn $ "  expect:  " ++ show expected
  putStrLn $ "  program: " ++ prog
  putStrLn ""
  result <- try (runStringModeWithInput showReport mode prog inputs) :: IO (Either SomeException ())
  let (passed, msg) = case (result, expected) of
        (Left e, ShouldFail) -> (True, "expected error caught: " ++ shorten (show e))
        (Right _, ShouldPass) -> (True, "completed without error")
        (Left e, ShouldPass) -> (False, "unexpected error: " ++ shorten (show e))
        (Right _, ShouldFail) -> (False, "expected error but program completed")
  putStrLn ""
  putStrLn $ "  >>> " ++ (if passed then boldGreen "PASS" else boldRed "FAIL") ++ " - " ++ dim msg
  if showReport
    then case result of
      Left _ -> putStrLn (dim "  (no security report — execution aborted by monitor)")
      Right _ -> return ()
    else return ()
  return passed
  where
    shorten s = if length s > 250 then take 250 s ++ "..." else s
compareModes ::
  String -> String -> String -> [Value] -> (ModeOutcome, ModeOutcome, ModeOutcome) -> IO Bool
compareModes ident desc prog inputs (predStatic, predNsu, predPu) = do
  putStrLn ""
  putStrLn (boldCyan ("== " ++ ident ++ " :: " ++ desc ++ " =="))
  putStrLn $ "  program: " ++ prog
  putStrLn $ "  inputs:  " ++ show inputs
  putStrLn ""
  outStatic <- runStatic prog
  outNsu <- runMonitored DynamicNSU prog inputs
  outPu <- runMonitored DynamicPU prog inputs
  let okS = sameKind outStatic predStatic
      okN = sameKind outNsu predNsu
      okP = sameKind outPu predPu
  printRow "Static" outStatic predStatic okS
  printRow "NSU   " outNsu predNsu okN
  printRow "PU    " outPu predPu okP
  let allOk = okS && okN && okP
  putStrLn $ "  >>> " ++ (if allOk then boldGreen "all predictions matched" else boldRed "PREDICTION MISMATCH")
  return allOk
  where
    printRow label got predicted ok = do
      let mark = if ok then boldGreen "✓" else boldRed "✗"
          predTag = "(predicted " ++ outcomeKind predicted ++ ")"
      putStrLn $
        "  "
          ++ label
          ++ "  "
          ++ mark
          ++ "  "
          ++ padR 7 (outcomeKind got)
          ++ "  "
          ++ dim predTag
          ++ "  "
          ++ truncate' 70 (outcomeDetail got)
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    truncate' n s = if length s > n then take n s ++ "..." else s

runStatic :: String -> IO ModeOutcome
runStatic prog = case parseImp prog of
  Left err -> return (Stuck ("parse error: " ++ show err))
  Right (Program lat fns mainCmd) ->
    let vars = getVars mainCmd
        env = initEnv lat
        bot = head (latticeLevels lat)
     in return $ case cmdType lat fns vars env bot mainCmd of
          WellTyped _ _ -> Accept "well-typed"
          TypeError msg -> Reject (truncMsg msg)
  where
    truncMsg s = if length s > 120 then take 120 s ++ "..." else s

runMonitored :: ExecMode -> String -> [Value] -> IO ModeOutcome
runMonitored mode prog inputs = do
  result <-
    try (runCapture mode prog inputs) ::
      IO
        ( Either
            SomeException
            (MultiMemory, Labels, [(Level, Value)], Influences, Partials, SecurityLattice, [VarName])
        )
  return $ case result of
    Right (_, _, o, _, _, _, _) -> Accept ("output " ++ show (map snd o))
    Left e ->
      let msg = show e
       in if "Dynamic Monitor Exception:" `isPrefixOf` msg
            then Abort (truncMsg msg)
            else Stuck (truncMsg msg)
  where
    truncMsg s = if length s > 120 then take 120 s ++ "..." else s
