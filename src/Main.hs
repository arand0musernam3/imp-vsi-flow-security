module Main where

import Control.Exception (SomeException, try)
import Examples
import Imp
import Parser (parseImp)
import Types (TypeRes (..), cmdType, initEnv)

data Expectation = ShouldPass | ShouldFail
  deriving (Eq, Show)

-- Run a program silently and return the full execution result so test
-- runners can check values and print the security report.
runCapture ::
  ExecMode ->
  String ->
  [Value] ->
  IO (MultiMemory, Labels, [(Level, Value)], Influences, Partials, SecurityLattice, [VarName])
runCapture mode prog inputs = case parseImp prog of
  Left err -> error (show err)
  Right (Program lat fns mainCmd) ->
    let vars = getVars mainCmd
        cfg  = initialConfig lat mainCmd inputs
     in case evalF 1000 mode lat fns cfg of
          Finished mm labs o infl p -> return (mm, labs, o, infl, p, lat, vars)
          OutOfFuel                 -> error "OutOfFuel"

-- Static analysis only: parse, run cmdType, check whether it returns
-- WellTyped or TypeError. Does not execute the program — TypeError is
-- a returned value, not a thrown exception.
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

-- Value test: run the program and compare the outputs emitted on the
-- given lattice level (matched by level name, e.g. "low", "L1").
-- Pass showReport = False to suppress the post-run security report.
runValueTest ::
  String ->
  ExecMode ->
  String ->
  [Value] ->
  String ->
  [Value] ->
  Bool ->
  IO Bool
runValueTest name mode prog inputs levelName expected showReport = do
  putStrLn ""
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ bold "TEST: " ++ name
  putStrLn (dim (replicate 60 '-'))
  putStrLn $ "  mode:    " ++ show mode
  putStrLn $ "  inputs:  " ++ show inputs
  putStrLn $ "  level:   " ++ levelName
  putStrLn $ "  expect:  output = " ++ show expected
  putStrLn $ "  program: " ++ prog
  result <-
    try (runCapture mode prog inputs) ::
      IO
        ( Either
            SomeException
            (MultiMemory, Labels, [(Level, Value)], Influences, Partials, SecurityLattice, [VarName])
        )
  let (passed, msg) = case result of
        Right (_, _, o, _, _, lat, _)
          | not (any ((== levelName) . lName) (latticeLevels lat)) ->
              (False, "level " ++ levelName ++ " not in lattice")
          | otherwise ->
              let got = [v | (l, v) <- o, lName l == levelName]
               in if got == expected
                    then (True, "got " ++ show got)
                    else
                      ( False,
                        "expected "
                          ++ show expected
                          ++ ", got "
                          ++ show got
                      )
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

main :: IO ()
main = do
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  putStrLn (bold "  Dynamic Monitor Regression Tests")
  putStrLn (bold (replicate 60 '='))

  results <-
    sequence
      [ runTest
          "NSU on assignment (branch taken, secret=1)"
          DynamicNSU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
          [1]
          ShouldFail
          True,
        runTest
          "NSU on assignment (branch not taken, secret=0)"
          DynamicNSU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
          [0]
          ShouldPass
          True,
        runTest
          "Explicit flow on output (secret to low channel)"
          DynamicNSU
          "input(high, secret); output(low, secret)"
          [42]
          ShouldFail
          True,
        runTest
          "Output well-typed (public to low channel)"
          DynamicNSU
          "x := 7; output(low, x)"
          []
          ShouldPass
          True,
        runTest
          "Implicit flow on input (input under secret PC)"
          DynamicNSU
          "input(high, secret); if secret then input(low, x) else skip; output(low, x)"
          [1, 99]
          ShouldFail
          True,
        runTest
          "NSU on function return (high pc, public target)"
          DynamicNSU
          "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(low, x)"
          [1]
          ShouldFail
          True,
        runTest
          "Empty input tape (runtime error)"
          DynamicNSU
          "input(low, x); output(low, x)"
          []
          ShouldFail
          True,
        runTest
          "NSU on erase (conditional erase under secret PC)"
          DynamicNSU
          "input(high, secret); x := 5; if secret then erase(high, x) else skip; output(low, x)"
          [1]
          ShouldFail
          True,
        runTest
          "Unconditional erase at top level is accepted"
          DynamicNSU
          "x := 5; erase(high, x); output(high, x)"
          []
          ShouldPass
          True,
        runTest
          "Untyped mode runs without monitor checks"
          Untyped
          "input(high, secret); output(low, secret)"
          [42]
          ShouldPass
          True,
        staticTest
          "Input overwrites a value (static)"
          "input(high, h); x := h; input(low, x); output(low, x)"
          ShouldPass,
        runTest
          "Input overwrites a value (dynamic)"
          DynamicNSU
          "input(high, h); x := h; input(low, x); output(low, x)"
          [42, 99]
          ShouldPass
          True,
        -- Value-checking regression for the Call-arg fix: a high-labeled arg
        -- must reach the callee's high view. Before the fix, vals were read
        -- from m_pc=bottom view and the callee's parameter held 0 in every
        -- view, so this would have output [0] instead of [42].
        runValueTest
          "Call: high-labeled arg reaches callee's high view"
          DynamicNSU
          "def f(a) { skip } return a; input(high, secret); x := call f(secret); output(high, x)"
          [42]
          "high"
          [42]
          True,
        runValueTest
          "Call: low-labeled arg flows through unchanged"
          DynamicNSU
          "def f(a) { skip } return a; x := 7; y := call f(x); output(low, y)"
          []
          "low"
          [7]
          True,
        -- Static-side regressions for the function-summary soundness fix.
        -- Before the fix, computeSummaries left failed (argL, pcL) entries
        -- at the initial bottom retLevel, so callers got WellTyped with
        -- retLevel=bottom even when the body would leak.
        staticTest
          "Static rejects call whose body fails for these arg levels"
          "def f(a) { output(low, a) } return 0; input(high, secret); x := call f(secret)"
          ShouldFail,
        staticTest
          "Static accepts call when body type-checks for these arg levels"
          "def f(a) { skip } return a; x := 7; y := call f(x)"
          ShouldPass,
        -- Same function as the negative case above, but called with a
        -- low-labeled argument. The (f, [high], _) combo is unverified
        -- (and unreachable here), but (f, [low], bottom) is fine, so the
        -- call site must still type-check.
        staticTest
          "Static accepts call to f with arg levels that DO verify, even though other combos fail"
          "def f(a) { output(low, a) } return 0; x := 7; y := call f(x)"
          ShouldPass,
        runValueTest
          "Function locals default to bottom"
          DynamicNSU
          "def f(a) { tmp := a } return tmp; y := call f(7); output(bottom, y)"
          []
          "bottom"
          [7]
          True,
        staticTest
          "Run custom lattice program - diamond lattice (static)"
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); z := x + y; output(High, z)"
          ShouldPass,
        runTest
          "Run custom lattice program - diamond lattice (dynamic)"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); z := x + y; output(High, z)"
          [3, 4]
          ShouldPass
          True,
        runTest
          "Run custom lattice program - diamond lattice"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          [3, 4]
          ShouldFail
          True,
        -- LABEL RESET ON OVERWRITE
        -- Assignment sets labs x to pc ⊔ rhs-label; there is no floor.
        -- Overwriting a previously-high variable with a public constant
        -- therefore drops its label back to ⊥.
        runValueTest
          "Label reset: overwrite with constant clears high label"
          DynamicNSU
          "input(high, y); x := y; x := 7; output(bottom, x)"
          [42]
          "bottom"
          [7]
          True,
        -- After the overwrite resets labs x to low, a secret-PC assignment
        -- should be caught by NSU.  With labs x stuck at high the NSU check
        -- passes silently (not (high <= high) = false), hiding the violation.
        runTest
          "Label reset: NSU fires after overwrite resets label"
          DynamicNSU
          "input(high, y); x := y; x := 7; if y then x := 3 else skip"
          [1]
          ShouldFail
          True,
        -- Diamond lattice variants of the same two properties.
        runValueTest
          "Diamond: label reset on overwrite allows output to Low"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; output(Low, x)"
          [42]
          "Low"
          [5]
          True,
        -- NSU at incomparable level should always be caught regardless of the
        -- l_target formula (NSU guard uses labs x directly).
        runTest
          "Diamond: NSU at incomparable level is always caught"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, z); if z then x := 5 else skip"
          [42, 1]
          ShouldFail
          True,
        -- After overwriting x (label resets to Low), assigning to it under an
        -- L1 PC should trigger NSU.  With labs x stuck at L1 the check
        -- not (L1 <= L1) = false passes silently.
        runTest
          "Diamond: overwrite resets label so NSU fires under same-level PC"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; input(L1, z); if z then x := 3 else skip"
          [42, 1]
          ShouldFail
          True,
        -- L2 overwrite after L1 input: l_target should be L2 (not High = L1 ∨ L2)
        -- so the L2-channel output is accepted and reads the overwritten value.
        runValueTest
          "Diamond: L2 overwrite after L1 data, output to L2"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); x := y; output(L2, x)"
          [42, 7]
          "L2"
          [7]
          True,
        staticTest
          "Leaky conditional erasure"
          "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
          ShouldFail,
        runValueTest
          "Leaky conditional erasure - safe dynamic path"
          DynamicNSU
          "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
          [1, 42]
          "low"
          [42]
          True,
        runTest
          "Leaky conditional erasure - vulnerable dynamic path"
          DynamicNSU
          "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
          [0, 42]
          ShouldFail
          True,
        -- DEEP ERASURE TESTS -- TODO currently there is no way to automatically check the contents of a variable on a specific label, it requires a manuall check (since checking if output(high, x) gives 0 throws an error, for x that was erased to top)

        runValueTest
          "Deep Erasure: data flow (x depends on y)"
          DynamicNSU
          "input(high, y); x := y; erase(top, y); output(top, x)"
          [42]
          "high"
          [] -- x should be erased because it depends on y
          True,
        runValueTest
          "Deep Erasure: control flow (x depends on y via PC)"
          DynamicNSU
          "input(high, y); x := y; if y then x := 1 else skip; erase(top, y); output(top, x)"
          [1]
          "high"
          [] -- x should be erased because it was assigned under a PC influenced by y
          True,
        runValueTest
          "Deep Erasure: dependency removal on overwrite"
          DynamicNSU
          "input(high, y); x := y; x := 7; erase(high, y); output(bottom, x)"
          [42]
          "bottom"
          [7] -- x should NOT be erased because it was overwritten
          True,
        staticTest
          "Deep Erasure: static rejection of leak after erasure"
          "input(high, y); x := y; erase(high, y); output(low, x)"
          ShouldFail, -- x is raised to high by deep erasure, so output(low) fails
        runValueTest
          "Deep Erasure: transitivity (z depends on x depends on y)"
          DynamicNSU
          "input(high, y); x := y; z := x; erase(top, y); output(top, z)"
          [42]
          "high"
          [] -- z should be erased
          True,
        -- MORE EDGE CASES
        runValueTest
          "Deep Erasure: circular dependency (x:=y; y:=x; erase y)"
          DynamicNSU
          "input(high, y); x := y; y := x; erase(top, y); output(top, x)"
          [42]
          "high"
          [] -- x should be erased because it transitively depends on y
          True,
        runValueTest
          "Deep Erasure: multiple influences (x:=y+z; erase y)"
          DynamicNSU
          "input(high, y); input(high, z); x := y + z; erase(top, y); output(top, x)"
          [10, 20]
          "high"
          [] -- x should be erased because it depends on y (even though z is fine)
          True,
        runValueTest
          "Deep Erasure: erasure inside function affects caller result"
          DynamicNSU
          "def f(a) { erase(top, a) } return a; input(high, y); x := call f(y); output(top, x)"
          [42]
          "high"
          [] -- x should be 0 because 'a' was erased inside the function before return
          True,
        runValueTest
          "Deep Erasure: while loop dependency (x incremented based on y)"
          DynamicNSU
          "input(high, y); x := y; while y do (x := x + 1; y := y - 1); erase(top, y); output(top, x)"
          [3]
          "high"
          [] -- x was modified under PC influenced by y, so it depends on y
          True,
        runValueTest
          "Deep Erasure: conditional erasure (erase only if z is true)"
          DynamicNSU
          "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(high, x)"
          [42, 0]
          "high"
          [42] -- z is false, so no erasure should happen
          True,
        runTest
          "Deep Erasure: conditional erasure (taken)"
          DynamicNSU
          "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(high, x)"
          [42, 1]
          ShouldFail
          True,
        runValueTest
          "Deep Erasure: prevent erasure after influencing varuable overwrite"
          DynamicNSU
          "input(high, c); x:= 5; v := x; input(high, x); if c then erase(top, x) else skip; output(low, v)"
          [1, 10]
          "low"
          [5]
          True,
        staticTest
          "While test"
          "input(high,c); x := 5; while c do x := x + 1"
          ShouldPass,
        ---------------------------------------------------------------------
        -- INFLUENCE-MAP CLEANUP: tests for the conditional cleanup rule.
        -- The rule: at `Assign x e`, drop x from every other variable's
        -- dependency set ONLY when the new x is independent of the old x
        -- (i.e. x ∉ inflClosure(varsExpr e, infl)). For a reflexive update
        -- like `x := x + 1`, x ∈ closure, so dependencies must be kept.
        ---------------------------------------------------------------------

        -- Reflexive update keeps the x->a link. Reading at top sees the old
        -- value (1) since eraseMultiMemory preserves the target-level view.
        runValueTest
          "Infl cleanup: reflexive x := x+1 keeps a's dep on x"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(high, x); output(high, a)"
          []
          "high"
          [1]
          True,
        -- Same setup; since erase raised labs a to top, reading at low must
        -- fail the dynamic monitor (label/channel mismatch).
        runTest
          "Infl cleanup: reflexive update + erase raises a to top (low output blocked)"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(top, x); output(low, a)"
          []
          ShouldFail
          True,
        -- Non-reflexive overwrite (`x := 5`) breaks the x->a link, so a is
        -- preserved and stays at its original (bottom) label.
        runValueTest
          "Infl cleanup: non-reflexive `x := 5` breaks the x->a influence"
          DynamicNSU
          "x := 1; a := x; x := 5; erase(top, x); output(low, a)"
          []
          "low"
          [1]
          True,
        -- a's label is high (set when secret was high), so output(low, a)
        -- fails. This shows that label-reset only happens for the *overwritten*
        -- variable, not for variables that previously read from it.
        runTest
          "Infl cleanup: non-reflexive overwrite keeps a at high"
          DynamicNSU
          "input(high, secret); a := secret; secret := 0; output(low, a)"
          [42]
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- EAGER CLOSURE: tests that dependency chains survive across
        -- intermediate variables, so a single erase finds all transitive
        -- descendants in one shot.
        ---------------------------------------------------------------------

        runValueTest
          "Eager closure: erase propagates through a 4-step chain"
          DynamicNSU
          "input(high, x); y := x; z := y; w := z; erase(high, x); output(high, w)"
          [42]
          "high"
          [42]
          True,
        runTest
          "Eager closure: every intermediate var rises to top"
          DynamicNSU
          "input(high, x); y := x; z := y; w := z; erase(top, x); output(high, w)"
          [42]
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- ERASE PRESERVES DEPENDENCIES two consecutive erases must both pick up the same dependent.
        ---------------------------------------------------------------------

        runValueTest
          "Erase chain: second erase still finds a via x"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(high, x); erase(top, x); output(top, a)"
          []
          "top"
          [1]
          True,
        runTest
          "Erase chain: a's final label is top after both erases"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(high, x); erase(top, x); output(high, a)"
          []
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- CALLEE->CALLER MAPPING: the return value's deps are
        -- traced through the callee's influence map and resolved back to
        -- the caller-side variables that fed each parameter.
        ---------------------------------------------------------------------

        -- Function does intermediate work; the eager closure inside the
        -- callee keeps t's chain back to a, and Return substitutes
        -- a -> caller's x. erase(top, x) must then also erase y.
        runValueTest
          "Callee chain a->t carries to caller y"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(low, x); y := call f(x); erase(top, x); output(top, y)"
          [10]
          "top"
          [11]
          True,
        runTest
          "y's label rises to top with x"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(low, x); y := call f(x); erase(top, x); output(low, y)"
          [10]
          ShouldFail
          True,
        -- If the function ignores its argument and returns a constant, the
        -- return-value-deps closure is empty, resolved is empty, and y
        -- depends on nothing. erase(top, x) must NOT touch y.
        runValueTest
          "y independent of x when f returns constant"
          DynamicNSU
          "def f(a) { skip } return 42; input(low, x); y := call f(x); erase(top, x); output(low, y)"
          [10]
          "low"
          [42]
          True,
        -- Self-call x := call f(x): the new x reaches resolved through
        -- the args, so caller's old x-deps must be preserved.
        runValueTest
          "self-call x := call f(x) keeps x's stream"
          DynamicNSU
          "def f(a) { skip } return a; input(low, x); a := x; x := call f(x); erase(top, x); output(top, a)"
          [7]
          "top"
          [7]
          True,
        runTest
          "a rises with x in self-call scenario"
          DynamicNSU
          "def f(a) { skip } return a; input(low, x); a := x; x := call f(x); erase(top, x); output(low, a)"
          [7]
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- FUNCTION LOCALS AT CALLER'S PC: a callee body that assigns to a
        -- local can now run under a high caller PC, because non-arg locals
        -- are initialized to `pc` rather than ⊥ (PLAS '09 [U-REF] analog).
        ---------------------------------------------------------------------

        -- The call is under high pc and the result is assigned to a target
        -- whose label is already high, so the return-side NSU also passes.
        runValueTest
          "Function locals at pc: body assigns under high caller pc"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(high, s); y := s; if s then y := call f(s) else skip; output(high, y)"
          [1]
          "high"
          [2]
          True,
        runTest
          "Function locals at pc: body assigns under high caller pc"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(high, s); erase(high, y); if s then y := call f(3) else skip; output(high, y)"
          [1]
          ShouldPass
          True,
        runTest
          "Function locals at pc: body assigns under high caller pc"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(high, s); if s then y := call f(3) else skip; output(low, y)"
          [1]
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- STATIC WHILE: fixed-point iteration accepts loops that raise
        -- variables' labels in the body. The pre-fix single-pass check
        -- rejected anything where env_out > env_in.
        ---------------------------------------------------------------------

        staticTest
          "Static While: high-PC body that raises x is accepted"
          "input(high, c); x := 5; while c do x := x + 1; output(high, x)"
          ShouldPass,
        staticTest
          "Static While: leak through loop body is still rejected"
          "input(high, c); x := 5; while c do x := x + 1; output(low, x)"
          ShouldFail,
        staticTest
          "Static While: multi-var loop reaches fixed point"
          "input(high, c); x := 0; y := 0; while c do (x := x + 1; y := y + x); output(high, y); output(high, x)"
          ShouldPass,
        staticTest
          "Static While: trivial loop (no env change) accepted"
          "input(low, c); x := 5; while c do skip; output(low, x)"
          ShouldPass,
        ---------------------------------------------------------------------
        -- PERMISSIVE UPGRADE: PU accepts a strict superset of NSU-accepted
        -- programs. Where NSU aborts at the upgrade site, PU allows the
        -- write, raises the target's label to pc ⊔ ℓ_e, and records the
        -- target in P. The deferred abort fires at a later branch that
        -- reads a P-marked variable.
        ---------------------------------------------------------------------

        -- Classical PU example. Under NSU this same program ShouldFail
        -- (see "NSU on assignment (branch taken, secret=1)" earlier).
        -- Under PU the upgrade x is allowed; x ends up at label high with
        -- x ∈ P; output(high, x) is fine because the channel dominates.
        runValueTest
          "PU accepts where NSU rejects (output at high)"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(high, x)"
          [1]
          "high"
          [1]
          True,
        -- PU does NOT compromise non-interference at outputs: even though
        -- the upgrade is allowed, x's raised label still blocks an output
        -- at a non-dominating channel.
        runTest
          "PU still catches the leak via labels (output at low)"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
          [1]
          ShouldFail
          True,
        -- The headline deferred-abort case: branching on x after x was
        -- upgraded into P aborts under PU.
        runTest
          "PU rejects branch on P-marked variable"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [1]
          ShouldFail
          True,
        -- Same program as the accept-case above with the opposite input.
        -- NSU rejects regardless of input (path-insensitive); PU accepts
        -- both paths uniformly.
        runValueTest
          "PU accepts the false-branch case symmetrically"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(high, x)"
          [0]
          "high"
          [0]
          True,
        -- Direct flow leak is caught by the channel/label flow check, not
        -- by NSU or PU. Both modes reject this.
        runTest
          "PU agrees with NSU on a direct flow leak"
          DynamicPU
          "input(high, secret); output(low, secret)"
          [42]
          ShouldFail
          True,
        -- PU permissiveness extends to the function-return NSU check. The
        -- same program is ShouldFail at "NSU on function return".
        runValueTest
          "PU permissive at function return"
          DynamicPU
          "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(high, x)"
          [1]
          "high"
          [0]
          True,
        ---------------------------------------------------------------------
        -- INFLUENCE-MAP CLEANUP IN CONDITIONAL CONTEXT.
        -- pc_vars must be carried into the new dep set, even when cleaning.
        ---------------------------------------------------------------------

        -- Under if c then x := ..., the new x's deps must include c (via
        -- pc_vars). erase(top, c) should then also erase x.
        runValueTest
          "Implicit flow: erase(c) deep-erases x assigned under PC c"
          DynamicNSU
          "input(high, c); input(high, x); if c then x := x+1 else skip; erase(top, c); output(top, x)"
          [1, 1]
          "top"
          [2]
          True,
        runTest
          "Implicit flow: x's label rises with c after deep erase"
          DynamicNSU
          "input(high, c); input(high, x); if c then x := x+1 else skip; erase(top, c); output(high, x)"
          [1, 1]
          ShouldFail
          True,
        runTest
          "Implicit flow leak"
          DynamicNSU
          "input(high, x); y:=1; z:=1; if x then input(high, y) else skip; if y then z:=0 else skip; output(low, z)"
          [1, 0]
          ShouldFail
          True,
        staticTest
          "The environment of variable restarts"
          "z := 1; input(top, a); input(high, s); if s then input(high, a) else skip; if a then z := 0 else skip; output(low, z)"
          ShouldFail,
        staticTest
          "The environment of variable restarts (2)"
          "z := 1; input(top, a); input(high, a); if a then z := 0 else skip; output(low, z)"
          ShouldFail
      ]

  -- TODO add more examples with custom lattices.
  -- TODO refine how the output tape works, which channel is it reading from? Perhaps we should have separate tapes, one for each lattice level.

  let passed = length (filter id results)
      total = length results
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  let summaryColor = if passed == total then boldGreen else boldRed
  putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed")
  putStrLn (bold (replicate 60 '='))
