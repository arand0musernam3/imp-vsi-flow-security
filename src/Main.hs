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
            initLabs  = \_ -> bot
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
    putStrLn (dim (replicate 60 '-'))
    putStrLn $ bold "TEST: " ++ name
    putStrLn (dim (replicate 60 '-'))
    putStrLn   "  mode:    Static (type-check only)"
    putStrLn $ "  expect:  " ++ show expected
    putStrLn $ "  program: " ++ prog
    let result = case parseImp prog of
            Left err -> Left ("parse error: " ++ show err)
            Right (Program lat fns mainCmd) ->
                let vars = getVars mainCmd
                    env  = initEnv lat
                    bot  = head (latticeLevels lat)
                in case cmdType lat fns vars env bot mainCmd of
                    WellTyped _ _ -> Right ()
                    TypeError msg -> Left msg
    let (passed, msg) = case (result, expected) of
            (Left e,  ShouldFail) -> (True,  "rejected: " ++ shortenS e)
            (Right _, ShouldPass) -> (True,  "well-typed")
            (Left e,  ShouldPass) -> (False, "unexpected rejection: " ++ shortenS e)
            (Right _, ShouldFail) -> (False, "expected rejection but well-typed")
    putStrLn $ "  >>> " ++ (if passed then boldGreen "PASS" else boldRed "FAIL") ++ " - " ++ dim msg
    return passed
  where
    shortenS s = if length s > 250 then take 250 s ++ "..." else s

-- Value test: run the program, compare output tape values, then show the
-- security report. Pass showReport = False to suppress the report.
runValueTest :: String -> ExecMode -> String -> [Value] -> [Value] -> Bool -> IO Bool
runValueTest name mode prog inputs expected showReport = do
    putStrLn ""
    putStrLn (dim (replicate 60 '-'))
    putStrLn $ bold "TEST: " ++ name
    putStrLn (dim (replicate 60 '-'))
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
    putStrLn $ "  >>> " ++ (if passed then boldGreen "PASS" else boldRed "FAIL") ++ " - " ++ dim msg
    if showReport
        then case result of
                Right (mm, labs, o, infl, lat, vars) ->
                    printSecurityReport lat vars mm labs o infl
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
            (Left e,  ShouldFail) -> (True,  "expected error caught: " ++ shorten (show e))
            (Right _, ShouldPass) -> (True,  "completed without error")
            (Left e,  ShouldPass) -> (False, "unexpected error: " ++ shorten (show e))
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

main :: IO ()
main = do
    putStrLn ""
    putStrLn (bold (replicate 60 '='))
    putStrLn (bold "  Dynamic Monitor Regression Tests")
    putStrLn (bold (replicate 60 '='))

    results <- sequence
        [ runTest "NSU on assignment (branch taken, secret=1)"
                  Dynamic
                  "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
                  [1]
                  ShouldFail
                  True

        , runTest "NSU on assignment (branch not taken, secret=0)"
                  Dynamic
                  "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
                  [0]
                  ShouldPass
                  True

        , runTest "Explicit flow on output (secret to low channel)"
                  Dynamic
                  "input(high, secret); output(low, secret)"
                  [42]
                  ShouldFail
                  True

        , runTest "Output well-typed (public to low channel)"
                  Dynamic
                  "x := 7; output(low, x)"
                  []
                  ShouldPass
                  True

        , runTest "Implicit flow on input (input under secret PC)"
                  Dynamic
                  "input(high, secret); if secret then input(low, x) else skip; output(low, x)"
                  [1, 99]
                  ShouldFail
                  True

        , runTest "NSU on function return (high pc, public target)"
                  Dynamic
                  "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(low, x)"
                  [1]
                  ShouldFail
                  True

        , runTest "Empty input tape (runtime error)"
                  Dynamic
                  "input(low, x); output(low, x)"
                  []
                  ShouldFail
                  True

        , runTest "NSU on erase (conditional erase under secret PC)"
                  Dynamic
                  "input(high, secret); x := 5; if secret then erase(high, x) else skip; output(low, x)"
                  [1]
                  ShouldFail
                  True

        , runTest "Unconditional erase at top level is accepted"
                  Dynamic
                  "x := 5; erase(high, x); output(high, x)"
                  []
                  ShouldPass
                  True

        , runTest "Untyped mode runs without monitor checks"
                  Untyped
                  "input(high, secret); output(low, secret)"
                  [42]
                  ShouldPass
                  True

        -- Value-checking regression for the Call-arg fix: a high-labeled arg
        -- must reach the callee's high view. Before the fix, vals were read
        -- from m_pc=bottom view and the callee's parameter held 0 in every
        -- view, so this would have output [0] instead of [42].
        , runValueTest "Call: high-labeled arg reaches callee's high view"
                       Dynamic
                       "def f(a) { skip } return a; input(high, secret); x := call f(secret); output(high, x)"
                       [42]
                       [42]
                       True

        , runValueTest "Call: low-labeled arg flows through unchanged"
                       Dynamic
                       "def f(a) { skip } return a; x := 7; y := call f(x); output(low, y)"
                       []
                       [7]
                       True

        -- Static-side regressions for the function-summary soundness fix.
        -- Before the fix, computeSummaries left failed (argL, pcL) entries
        -- at the initial bottom retLevel, so callers got WellTyped with
        -- retLevel=bottom even when the body would leak.
        , runStaticTest "Static rejects call whose body fails for these arg levels"
                        "def f(a) { output(low, a) } return 0; input(high, secret); x := call f(secret)"
                        ShouldFail

        , runStaticTest "Static accepts call when body type-checks for these arg levels"
                        "def f(a) { skip } return a; x := 7; y := call f(x)"
                        ShouldPass

        -- Same function as the negative case above, but called with a
        -- low-labeled argument. The (f, [high], _) combo is unverified
        -- (and unreachable here), but (f, [low], bottom) is fine, so the
        -- call site must still type-check.
        , runStaticTest "Static accepts call to f with arg levels that DO verify, even though other combos fail"
                        "def f(a) { output(low, a) } return 0; x := 7; y := call f(x)"
                        ShouldPass

        -- `stop` semantics: it must halt the entire program from any
        -- syntactic position. Before introducing Halt, the user's `stop`
        -- and the internal "command finished" marker were the same
        -- constructor, and `Seq` could not distinguish them — so any
        -- `stop` followed by code (or surfacing from an if-branch, or
        -- nested in a function body) crashed with `step Stop = error`.
        , runValueTest "stop alone halts cleanly"
                       Dynamic
                       "x := 1; output(low, x); stop"
                       []
                       [1]
                       True

        , runValueTest "stop in middle of sequence suppresses subsequent code"
                       Dynamic
                       "x := 1; output(low, x); stop; output(low, 99)"
                       []
                       [1]
                       True

        , runValueTest "stop in if-branch halts (taken branch)"
                       Dynamic
                       "x := 1; output(low, x); if x then stop else skip; output(low, 99)"
                       []
                       [1]
                       True

        , runValueTest "stop inside function halts the entire program"
                       Dynamic
                       "def f(a) { output(low, a); stop } return 0; y := call f(7); output(low, 99)"
                       []
                       [7]
                       True

        , runValueTest "Function locals default to bottom"
                       Dynamic
                       "def f(a) { tmp := a } return tmp; y := call f(7); output(low, y)"
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
        -- LABEL RESET ON OVERWRITE
        -- Assignment sets labs x to pc ⊔ rhs-label; there is no floor.
        -- Overwriting a previously-high variable with a public constant
        -- therefore drops its label back to ⊥.
        , runValueTest "Label reset: overwrite with constant clears high label"
                       Dynamic
                       "input(high, y); x := y; x := 7; output(low, x)"
                       [42]
                       [7]
                       True

        -- After the overwrite resets labs x to low, a secret-PC assignment
        -- should be caught by NSU.  With labs x stuck at high the NSU check
        -- passes silently (not (high <= high) = false), hiding the violation.
        , runTest "Label reset: NSU fires after overwrite resets label"
                  Dynamic
                  "input(high, y); x := y; x := 7; if y then x := 3 else skip"
                  [1]
                  ShouldFail
                  True

        -- Diamond lattice variants of the same two properties.
        , runValueTest "Diamond: label reset on overwrite allows output to Low"
                       Dynamic
                       "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; output(Low, x)"
                       [42]
                       [5]
                       True

        -- NSU at incomparable level should always be caught regardless of the
        -- l_target formula (NSU guard uses labs x directly).
        , runTest "Diamond: NSU at incomparable level is always caught"
                  Dynamic
                  "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, z); if z then x := 5 else skip"
                  [42, 1]
                  ShouldFail
                  True

        -- After overwriting x (label resets to Low), assigning to it under an
        -- L1 PC should trigger NSU.  With labs x stuck at L1 the check
        -- not (L1 <= L1) = false passes silently.
        , runTest "Diamond: overwrite resets label so NSU fires under same-level PC"
                  Dynamic
                  "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; input(L1, z); if z then x := 3 else skip"
                  [42, 1]
                  ShouldFail
                  True

        -- L2 overwrite after L1 input: l_target should be L2 (not High = L1 ∨ L2)
        -- so the L2-channel output is accepted and reads the overwritten value.
        , runValueTest "Diamond: L2 overwrite after L1 data, output to L2"
                       Dynamic
                       "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); x := y; output(L2, x)"
                       [42, 7]
                       [7]
                       True

        , runStaticTest "Leaky conditional erasure"
                "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
                ShouldFail
        , runValueTest "Leaky conditional erasure - safe dynamic path"
                Dynamic
                "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
                [1, 42]
                [42]
                True
        , runTest "Leaky conditional erasure - vulnerable dynamic path"
                Dynamic
                "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
                [0, 42]
                ShouldFail
                True
                

        -- DEEP ERASURE TESTS -- TODO currently there is no way to automatically check the contents of a variable on a specific label, it requires a manuall check (since checking if output(high, x) gives 0 throws an error, for x that was erased to top)
        
        , runValueTest "Deep Erasure: data flow (x depends on y)"
                Dynamic
                "input(high, y); x := y; erase(top, y); output(top, x)"
                [42]
                [42]  -- x should be erased because it depends on y
                True

        , runValueTest "Deep Erasure: control flow (x depends on y via PC)"
                       Dynamic
                       "input(high, y); x := y; if y then x := 1 else skip; erase(top, y); output(top, x)"
                       [1]
                       [1]  -- x should be erased because it was assigned under a PC influenced by y
                       True

        , runValueTest "Deep Erasure: dependency removal on overwrite"
                       Dynamic
                       "input(high, y); x := y; x := 7; erase(high, y); output(low, x)"
                       [42]
                       [7]  -- x should NOT be erased because it was overwritten
                       True

        , runStaticTest "Deep Erasure: static rejection of leak after erasure"
                        "input(high, y); x := y; erase(high, y); output(low, x)"
                        ShouldFail -- x is raised to high by deep erasure, so output(low) fails

        , runValueTest "Deep Erasure: transitivity (z depends on x depends on y)"
                       Dynamic
                       "input(high, y); x := y; z := x; erase(top, y); output(top, z)"
                       [42]
                       [42] -- z should be erased
                       True

        -- MORE EDGE CASES
        , runValueTest "Deep Erasure: circular dependency (x:=y; y:=x; erase y)"
                       Dynamic
                       "input(high, y); x := y; y := x; erase(top, y); output(top, x)"
                       [42]
                       [42] -- x should be erased because it transitively depends on y
                       True

        , runValueTest "Deep Erasure: multiple influences (x:=y+z; erase y)"
                       Dynamic
                       "input(high, y); input(high, z); x := y + z; erase(top, y); output(top, x)"
                       [10, 20]
                       [30] -- x should be erased because it depends on y (even though z is fine)
                       True

        , runValueTest "Deep Erasure: erasure inside function affects caller result"
                       Dynamic
                       "def f(a) { erase(top, a) } return a; input(high, y); x := call f(y); output(top, x)"
                       [42]
                       [42] -- x should be 0 because 'a' was erased inside the function before return
                       True

        , runValueTest "Deep Erasure: while loop dependency (x incremented based on y)"
                       Dynamic
                       "input(high, y); x := y; while y do (x := x + 1; y := y - 1); erase(top, y); output(top, x)"
                       [3]
                       [6] -- x was modified under PC influenced by y, so it depends on y
                       True

        , runValueTest "Deep Erasure: conditional erasure (erase only if z is true)"
                       Dynamic
                       "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(top, x)"
                       [42, 0]
                       [42] -- z is false, so no erasure should happen
                       True

        , runValueTest "Deep Erasure: conditional erasure (taken)"
                       Dynamic
                       "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(top, x)"
                       [42, 1]
                       [42] -- z is true, so x should be erased via y
                       True
        , runValueTest "Deep Erasure: prevent erasure after influencing varuable overwrite"
                       Dynamic
                       "input(high, c); x:= 5; v := x; input(high, x); if c then erase(top, x) else skip; output(low, v)"
                       [1, 10]
                       [5]
                       True
        , runValueTest "Calee infl map: bug testing"
                       Dynamic
                       "def f(a) { t := a; t := t + 1 } return t; input(low, x); y := call f(x); erase(top, x)"
                       [10]
                       []
                       True
        , runValueTest "Calee infl map: bug testing"
                        Dynamic
                        "def f(a) { t := a; t := t + 1 } return t; input(low, t); y := call f(t); erase(top, t)" 
                        [10]
                        []
                        True
        , runStaticTest "While test"
                        "input(high,c); x := 5; while c do x := x + 1"
                        ShouldPass
        , runValueTest "Functions can't make assignments under a high PC - bug"
                        Dynamic
                        "def f(x) { t := x } return t; input(high, s); if s then a := 5 else skip"
                        [1]
                        []
                        True
        ]


-- TODO add more examples with custom lattices.
-- TODO refine how the output tape works, which channel is it reading from? Perhaps we should have separate tapes, one for each lattice level.


    let passed = length (filter id results)
        total  = length results
    putStrLn ""
    putStrLn (bold (replicate 60 '='))
    let summaryColor = if passed == total then boldGreen else boldRed
    putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed")
    putStrLn (bold (replicate 60 '='))
