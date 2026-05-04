module Main where

import Control.Exception (SomeException, try)
import qualified Data.Map as Map
import Examples
import Imp
import Parser (parseImp)
import Types (cmdType, initEnv, TypeRes (..))
-- levelFromName comes from Imp (in scope via the unqualified Imp import above)

data Expectation = ShouldPass | ShouldFail
    deriving (Eq, Show)

-- Quietly run a program and return its output tape, so tests can assert on values.
quietRun :: ExecMode -> String -> [Value] -> IO [Value]
quietRun mode prog inputs = case parseImp prog of
    Left err -> error (show err)
    Right (Program lat fns mainCmd) ->
        let bot       = head (latticeLevels lat)
            initLabs  = levelFromName lat
            initialMM = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ]
        in case evalF 1000 mode lat fns
                       (mainCmd, initialMM, initLabs, [bot], inputs, [], []) of
            Finished _ _ o -> return o
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
                    WellTyped _   -> Right ()
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

runValueTest :: String -> ExecMode -> String -> [Value] -> [Value] -> IO Bool
runValueTest name mode prog inputs expected = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn $ "  mode:    " ++ show mode
    putStrLn $ "  inputs:  " ++ show inputs
    putStrLn $ "  expect:  output = " ++ show expected
    putStrLn $ "  program: " ++ prog
    result <- try (quietRun mode prog inputs) :: IO (Either SomeException [Value])
    let (passed, msg) = case result of
            Right out
              | out == expected -> (True,  "got " ++ show out)
              | otherwise       -> (False, "expected " ++ show expected ++ ", got " ++ show out)
            Left e -> (False, "unexpected error: " ++ shortenV (show e))
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
    return passed
  where
    shortenV s = if length s > 250 then take 250 s ++ "..." else s

runTest :: String -> ExecMode -> String -> [Value] -> Expectation -> IO Bool
runTest name mode prog inputs expected = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn $ "  mode:    " ++ show mode
    putStrLn $ "  inputs:  " ++ show inputs
    putStrLn $ "  expect:  " ++ show expected
    putStrLn $ "  program: " ++ prog
    putStrLn ""
    result <- try (runStringModeWithInput mode prog inputs) :: IO (Either SomeException ())
    let (passed, msg) = case (result, expected) of
            (Left e,  ShouldFail) -> (True,  "expected error caught: " ++ shorten (show e))
            (Right _, ShouldPass) -> (True,  "completed without error")
            (Left e,  ShouldPass) -> (False, "unexpected error: " ++ shorten (show e))
            (Right _, ShouldFail) -> (False, "expected error but program completed")
    putStrLn ""
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
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

        , runTest "NSU on assignment (branch not taken, secret=0)"
                  Dynamic
                  "input(high, secret_s); x_p := 0; if secret_s then x_p := 1 else skip; output(low, x_p)"
                  [0]
                  ShouldPass

        , runTest "Explicit flow on output (secret to low channel)"
                  Dynamic
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldFail

        , runTest "Output well-typed (public to low channel)"
                  Dynamic
                  "x_p := 7; output(low, x_p)"
                  []
                  ShouldPass

        , runTest "Implicit flow on input (input under secret PC)"
                  Dynamic
                  "input(high, secret_s); if secret_s then input(low, x_p) else skip; output(low, x_p)"
                  [1, 99]
                  ShouldFail

        , runTest "NSU on function return (high pc, public target)"
                  Dynamic
                  "def f(a) { skip } return a; input(high, secret_s); x_p := 0; if secret_s then x_p := call f(0) else skip; output(low, x_p)"
                  [1]
                  ShouldFail

        , runTest "Empty input tape (runtime error)"
                  Dynamic
                  "input(low, x_p); output(low, x_p)"
                  []
                  ShouldFail

        , runTest "NSU on erase (conditional erase under secret PC)"
                  Dynamic
                  "input(high, secret_s); x_p := 5; if secret_s then erase(high, x_p) else skip; output(low, x_p)"
                  [1]
                  ShouldFail

        , runTest "Unconditional erase at top level is accepted"
                  Dynamic
                  "x_p := 5; erase(high, x_p); output(high, x_p)"
                  []
                  ShouldPass

        , runTest "Untyped mode runs without monitor checks"
                  Untyped
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldPass

        -- Value-checking regression for the Call-arg fix: a high-labeled arg
        -- must reach the callee's high view. Before the fix, vals were read
        -- from m_pc=bottom view and the callee's parameter held 0 in every
        -- view, so this would have output [0] instead of [42].
        , runValueTest "Call: high-labeled arg reaches callee's high view"
                       Dynamic
                       "def f(a) { skip } return a; input(high, secret_s); x_s := call f(secret_s); output(high, x_s)"
                       [42]
                       [42]

        , runValueTest "Call: low-labeled arg flows through unchanged"
                       Dynamic
                       "def f(a) { skip } return a; x_p := 7; y_p := call f(x_p); output(low, y_p)"
                       []
                       [7]

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

        , runValueTest "stop in middle of sequence suppresses subsequent code"
                       Dynamic
                       "x_p := 1; output(low, x_p); stop; output(low, 99)"
                       []
                       [1]

        , runValueTest "stop in if-branch halts (taken branch)"
                       Dynamic
                       "x_p := 1; output(low, x_p); if x_p then stop else skip; output(low, 99)"
                       []
                       [1]

        , runValueTest "stop inside function halts the entire program"
                       Dynamic
                       "def f(a) { output(low, a); stop } return 0; y_p := call f(7); output(low, 99)"
                       []
                       [7]

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

        -- _s local: emitting it on a low channel must be rejected. Before
        -- the fix, labs secret_s started at bottom, so the output check
        -- (bottom ⊔ pc) ⊑ low passed and the program ran silently with
        -- the value 0 from the local's freshly-zeroed memory.
        , runTest "Function _s local follows naming convention (high initial label, low output rejected)"
                  Dynamic
                  "def f() { output(low, secret_s) } return 0; y_p := call f()"
                  []
                  ShouldFail

        -- Sanity: a function whose locals have no _p/_s suffix is
        -- unaffected by the naming-convention change.
        , runValueTest "Function with bottom-labeled locals still works"
                       Dynamic
                       "def f(a) { tmp := a } return tmp; y_p := call f(7); output(low, y_p)"
                       []
                       [7]
        ]

    let passed = length (filter id results)
        total  = length results
    putStrLn ""
    putStrLn (replicate 60 '=')
    putStrLn $ "  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed"
    putStrLn (replicate 60 '=')
