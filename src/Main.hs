module Main where

import Control.Exception (SomeException, try)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Examples
import Imp
import Parser (parseImp)
import Types (cmdType, initEnv, TypeRes (..))

data Expectation = ShouldPass | ShouldFail
    deriving (Eq, Show)

-- Run a program silently and return the full execution result so test
-- runners can check values and print the security report.
runCapture :: ExecMode -> String -> [Value]
           -> IO (MultiMemory, Labels, [(Level, Value)], Influences, SecurityLattice, [VarName])
runCapture mode prog inputs = case parseImp prog of
    Left err -> error (show err)
    Right (Program lat fns mainCmd) ->
        let bot       = head (latticeLevels lat)
            initLabs  = levelFromName lat
            initialMM = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
            initialPC = (bot, Set.empty)
            initialInfluences = Map.empty
            vars      = getVars mainCmd
        in case evalF 1000 mode lat fns -- 1000 fuel
                       (mainCmd, initialMM, initLabs, [initialPC], inputs, [], [], initialInfluences) of
            Finished mm labs o infl -> return (mm, labs, o, infl, lat, vars)
            OutOfFuel      -> error "OutOfFuel"

-- Static-only test: parse, run cmdType, check whether it returns
-- WellTyped or TypeError. Bypasses runStringModeWithInput because
-- TypeError is a returned value, not a thrown exception.
runStaticTest :: String -> String -> Expectation -> IO Bool
runStaticTest name prog expected = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn   "  mode:    Static (type-check only)"
    putStrLn $ "  expect:  " ++ show expected
    putStrLn $ "  program: " ++ prog
    let result = case parseImp prog of
            Left err -> Left ("parse error: " ++ show err)
            Right (Program lat fns mainCmd) ->
                let vars = getVars mainCmd
                    env  = initEnv lat vars
                    bot  = head (latticeLevels lat)
                in case cmdType lat fns vars env bot mainCmd of
                    WellTyped _ _ -> Right ()
                    TypeError msg -> Left msg
    let (passed, msg) = case (result, expected) of
            (Left e,  ShouldFail) -> (True,  "rejected: " ++ shortenS e)
            (Right _, ShouldPass) -> (True,  "well-typed")
            (Left e,  ShouldPass) -> (False, "unexpected rejection: " ++ shortenS e)
            (Right _, ShouldFail) -> (False, "expected rejection but well-typed")
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
    return passed
  where
    shortenS s = if length s > 250 then take 250 s ++ "..." else s

-- Value test: run the program, compare output tape values, then show the
-- security report. Pass showReport = False to suppress the report.
runValueTest :: String -> ExecMode -> String -> [Value] -> [Value] -> Bool -> IO Bool
runValueTest name mode prog inputs expected showReport = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn $ "  mode:    " ++ show mode
    putStrLn $ "  inputs:  " ++ show inputs
    putStrLn $ "  expect:  output = " ++ show expected
    putStrLn $ "  program: " ++ prog
    result <- try (runCapture mode prog inputs)
                :: IO (Either SomeException
                              (MultiMemory, Labels, [(Level, Value)], Influences, SecurityLattice, [VarName]))
    let (passed, msg) = case result of
            Right (_, _, o, _, _, _)
              | map snd o == expected -> (True,  "got " ++ show (map snd o))
              | otherwise             -> (False, "expected " ++ show expected
                                                 ++ ", got " ++ show (map snd o))
            Left e -> (False, "unexpected error: " ++ shorten (show e))
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
    if showReport
        then case result of
                Right (mm, labs, o, infl, lat, vars) ->
                    printSecurityReport lat vars mm labs o infl
                Left _ ->
                    putStrLn "  (no security report — execution terminated with error)"
        else return ()
    return passed
  where
    shorten s = if length s > 250 then take 250 s ++ "..." else s

runTest :: String -> ExecMode -> String -> [Value] -> Expectation -> Bool -> IO Bool
runTest name mode prog inputs expected showReport = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn $ "  mode:    " ++ show mode
    putStrLn $ "  inputs:  " ++ show inputs
    putStrLn $ "  expect:  " ++ show expected
    putStrLn $ "  program: " ++ prog
    putStrLn ""
    result <- try (runStringModeWithInput showReport mode prog inputs) :: IO (Either SomeException ())
    let (passed, msg) = case (result, expected) of
            (Left e,  ShouldFail) -> (True,  "expected error caught: " ++ shorten (show e))
            (Right _, ShouldPass) -> (True,  "completed without error")
            (Left e,  ShouldPass) -> (False, "unexpected error: " ++ shorten (show e))
            (Right _, ShouldFail) -> (False, "expected error but program completed")
    putStrLn ""
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
    if showReport
        then case result of
                Left _ -> putStrLn "  (no security report — execution aborted by monitor)"
                Right _ -> return ()
        else return ()
    return passed
  where
    shorten s = if length s > 250 then take 250 s ++ "..." else s

main :: IO ()
main = do
    putStrLn ""
    putStrLn (replicate 60 '=')
    putStrLn "  Dynamic Monitor Regression Tests"
    putStrLn (replicate 60 '=')

    results <- sequence
        [ runTest "NSU on assignment (branch taken, secret=1)"
                  Dynamic
                  "input(high, secret_s); x_p := 0; if secret_s then x_p := 1 else skip; output(low, x_p)"
                  [1]
                  ShouldFail
                  True

        , runTest "NSU on assignment (branch not taken, secret=0)"
                  Dynamic
                  "input(high, secret_s); x_p := 0; if secret_s then x_p := 1 else skip; output(low, x_p)"
                  [0]
                  ShouldPass
                  True

        , runTest "Explicit flow on output (secret to low channel)"
                  Dynamic
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldFail
                  True

        , runTest "Output well-typed (public to low channel)"
                  Dynamic
                  "x_p := 7; output(low, x_p)"
                  []
                  ShouldPass
                  True

        , runTest "Implicit flow on input (input under secret PC)"
                  Dynamic
                  "input(high, secret_s); if secret_s then input(low, x_p) else skip; output(low, x_p)"
                  [1, 99]
                  ShouldFail
                  True

        , runTest "NSU on function return (high pc, public target)"
                  Dynamic
                  "def f(a) { skip } return a; input(high, secret_s); x_p := 0; if secret_s then x_p := call f(0) else skip; output(low, x_p)"
                  [1]
                  ShouldFail
                  True

        , runTest "Empty input tape (runtime error)"
                  Dynamic
                  "input(low, x_p); output(low, x_p)"
                  []
                  ShouldFail
                  True

        , runTest "NSU on erase (conditional erase under secret PC)"
                  Dynamic
                  "input(high, secret_s); x_p := 5; if secret_s then erase(high, x_p) else skip; output(low, x_p)"
                  [1]
                  ShouldFail
                  True

        , runTest "Unconditional erase at top level is accepted"
                  Dynamic
                  "x_p := 5; erase(high, x_p); output(high, x_p)"
                  []
                  ShouldPass
                  True

        , runTest "Untyped mode runs without monitor checks"
                  Untyped
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldPass
                  True

        -- Value-checking regression for the Call-arg fix: a high-labeled arg
        -- must reach the callee's high view. Before the fix, vals were read
        -- from m_pc=bottom view and the callee's parameter held 0 in every
        -- view, so this would have output [0] instead of [42].
        , runValueTest "Call: high-labeled arg reaches callee's high view"
                       Dynamic
                       "def f(a) { skip } return a; input(high, secret_s); x_s := call f(secret_s); output(high, x_s)"
                       [42]
                       [42]
                       True

        , runValueTest "Call: low-labeled arg flows through unchanged"
                       Dynamic
                       "def f(a) { skip } return a; x_p := 7; y_p := call f(x_p); output(low, y_p)"
                       []
                       [7]
                       True

        -- Static-side regressions for the function-summary soundness fix.
        -- Before the fix, computeSummaries left failed (argL, pcL) entries
        -- at the initial bottom retLevel, so callers got WellTyped with
        -- retLevel=bottom even when the body would leak.
        , runStaticTest "Static rejects call whose body fails for these arg levels"
                        "def f(a) { output(low, a) } return 0; input(high, secret_s); x_p := call f(secret_s)"
                        ShouldFail

        , runStaticTest "Static accepts call when body type-checks for these arg levels"
                        "def f(a) { skip } return a; x_p := 7; y_p := call f(x_p)"
                        ShouldPass

        -- Same function as the negative case above, but called with a
        -- low-labeled argument. The (f, [high], _) combo is unverified
        -- (and unreachable here), but (f, [low], bottom) is fine, so the
        -- call site must still type-check.
        , runStaticTest "Static accepts call to f with arg levels that DO verify, even though other combos fail"
                        "def f(a) { output(low, a) } return 0; x_p := 7; y_p := call f(x_p)"
                        ShouldPass

        -- `stop` semantics: it must halt the entire program from any
        -- syntactic position. Before introducing Halt, the user's `stop`
        -- and the internal "command finished" marker were the same
        -- constructor, and `Seq` could not distinguish them — so any
        -- `stop` followed by code (or surfacing from an if-branch, or
        -- nested in a function body) crashed with `step Stop = error`.
        , runValueTest "stop alone halts cleanly"
                       Dynamic
                       "x_p := 1; output(low, x_p); stop"
                       []
                       [1]
                       True

        , runValueTest "stop in middle of sequence suppresses subsequent code"
                       Dynamic
                       "x_p := 1; output(low, x_p); stop; output(low, 99)"
                       []
                       [1]
                       True

        , runValueTest "stop in if-branch halts (taken branch)"
                       Dynamic
                       "x_p := 1; output(low, x_p); if x_p then stop else skip; output(low, 99)"
                       []
                       [1]
                       True

        , runValueTest "stop inside function halts the entire program"
                       Dynamic
                       "def f(a) { output(low, a); stop } return 0; y_p := call f(7); output(low, 99)"
                       []
                       [7]
                       True

        -- Function locals now follow the _p / _s naming convention for
        -- their initial label, matching how main-program variables are
        -- initialised. Before this fix, all non-parameter function locals
        -- defaulted to `bottom`, which was both inconsistent with main and
        -- subtly wrong (more restrictive than expected for _p, less
        -- restrictive than expected for _s).

        -- _p local: writing low data to it under a low PC is fine. Before
        -- the fix, NSU rejected because labs x_p = bottom and low ⊑ bottom
        -- is false.
        , runValueTest "Function _p local follows naming convention (NSU under low PC)"
                       Dynamic
                       "def f(arg) { x_p := arg } return x_p; input(low, y_p); if y_p then z_p := call f(y_p) else skip; output(low, z_p)"
                       [1]
                       [1]
                       True

        -- _s local: emitting it on a low channel must be rejected. Before
        -- the fix, labs secret_s started at bottom, so the output check
        -- (bottom ⊔ pc) ⊑ low passed and the program ran silently with
        -- the value 0 from the local's freshly-zeroed memory.
        , runTest "Function _s local follows naming convention (high initial label, low output rejected)"
                  Dynamic
                  "def f() { output(low, secret_s) } return 0; y_p := call f()"
                  []
                  ShouldFail
                  True

        -- Sanity: a function whose locals have no _p/_s suffix is
        -- unaffected by the naming-convention change.
        , runValueTest "Function with bottom-labeled locals still works"
                       Dynamic
                       "def f(a) { tmp := a } return tmp; y_p := call f(7); output(low, y_p)"
                       []
                       [7]
                       True

        , runTest "Run custom lattice program - diamond lattice"
                  Both
                  "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); z := x + y; output(High, z)"
                  [3, 4]
                  ShouldPass
                  True

        , runTest "Run custom lattice program - diamond lattice"
                  Dynamic
                  "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
                  [3, 4]
                  ShouldFail
                  True

        -- DEEP ERASURE TESTS
        
        , runValueTest "Deep Erasure: data flow (x depends on y)"
                Dynamic
                "input(high, y_s); x_s := y_s; erase(top, y_s); output(top, x_s)"
                [42]
                [0]  -- x_s should be erased because it depends on y_s
                True

        , runValueTest "Deep Erasure: control flow (x depends on y via PC)"
                       Dynamic
                       "input(high, y_s); x_s := y_s; if y_s then x_s := 1 else skip; erase(top, y_s); output(top, x_s)"
                       [1]
                       [0]  -- x_s should be erased because it was assigned under a PC influenced by y_s
                       True

        , runValueTest "Deep Erasure: dependency removal on overwrite"
                       Dynamic
                       "input(high, y_s); x_p := y_s; x_p := 7; erase(high, y_s); output(low, x_p)"
                       [42]
                       [7]  -- x_p should NOT be erased because it was overwritten
                       True

        , runStaticTest "Deep Erasure: static rejection of leak after erasure"
                        "input(high, y_s); x_p := y_s; erase(high, y_s); output(low, x_p)"
                        ShouldFail -- x_p is raised to high by deep erasure, so output(low) fails

        , runValueTest "Deep Erasure: transitivity (z depends on x depends on y)"
                       Dynamic
                       "input(high, y_s); x_s := y_s; z_s := x_s; erase(top, y_s); output(top, z_s)"
                       [42]
                       [0] -- z_s should be erased
                       True

        -- MORE EDGE CASES
        , runValueTest "Deep Erasure: circular dependency (x:=y; y:=x; erase y)"
                       Dynamic
                       "input(high, y_s); x_s := y_s; y_s := x_s; erase(top, y_s); output(top, x_s)"
                       [42]
                       [0] -- x_s should be erased because it transitively depends on y_s
                       True

        , runValueTest "Deep Erasure: multiple influences (x:=y+z; erase y)"
                       Dynamic
                       "input(high, y_s); input(high, z_s); x_s := y_s + z_s; erase(top, y_s); output(top, x_s)"
                       [10, 20]
                       [0] -- x_s should be erased because it depends on y_s (even though z_s is fine)
                       True

        , runValueTest "Deep Erasure: erasure inside function affects caller result"
                       Dynamic
                       "def f(a) { erase(top, a) } return a; input(high, y_s); x_s := call f(y_s); output(top, x_s)"
                       [42]
                       [0] -- x_s should be 0 because 'a' was erased inside the function before return
                       True

        , runValueTest "Deep Erasure: while loop dependency (x incremented based on y)"
                       Dynamic
                       "input(high, y_s); x_s := y_s; while y_s do (x_s := x_s + 1; y_s := y_s - 1); erase(top, y_s); output(top, x_s)"
                       [3]
                       [0] -- x_s was modified under PC influenced by y_s, so it depends on y_s
                       True

        , runValueTest "Deep Erasure: conditional erasure (erase only if z is true)"
                       Dynamic
                       "input(high, y_s); input(low, z_p); x_s := y_s; if z_p then erase(top, y_s) else skip; output(top, x_s)"
                       [42, 0]
                       [42] -- z_p is false, so no erasure should happen
                       True

        , runValueTest "Deep Erasure: conditional erasure (taken)"
                       Dynamic
                       "input(high, y_s); input(low, z_p); x_s := y_s; if z_p then erase(top, y_s) else skip; output(top, x_s)"
                       [42, 1]
                       [0] -- z_p is true, so x_s should be erased via y_s
                       True

        ]


-- TODO make examples with custom lattices.
-- TODO properly make sure that naming comvention is turned off for custom lattices and function parameters - (_s, _p).
-- TODO refine how the output tape works, which channel is it reading from? Perhaps we should have seperate tapes, one for each lattiice level?


    let passed = length (filter id results)
        total  = length results
    putStrLn ""
    putStrLn (replicate 60 '=')
    putStrLn $ "  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed"
    putStrLn (replicate 60 '=')
