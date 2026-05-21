module Main where

import Control.Exception (SomeException, try)
import Examples
import Imp
import Parser (parseImp)
import Types (TypeRes (..), cmdType, initEnv)

data Expectation = ShouldPass | ShouldFail
  deriving (Eq, Show)

-- Run a program silently; return the full execution state.
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

-- Static type-check only; does not execute. TypeError is a returned value,
-- not a thrown exception.
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

-- Run the program and compare what an observer at `observerName` sees.
-- An emission (ch, v) is visible to observer L iff ch ⊑ L. Same
-- semantics as the per-observer breakdown in printSecurityReport.
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
        runObserverTest
          "Call: high-labeled arg reaches callee's high view"
          DynamicNSU
          "def f(a) { skip } return a; input(high, secret); x := call f(secret); output(high, x)"
          [42]
          "high"
          [42]
          True,
        runObserverTest
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
        runObserverTest
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
        runObserverTest
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
        runObserverTest
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
        runObserverTest
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
        runObserverTest
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
        ---------------------------------------------------------------------
        -- DEEP ERASURE
        ---------------------------------------------------------------------

        runObserverTest
          "Deep Erasure: data flow (x depends on y)"
          DynamicNSU
          "input(high, y); x := y; erase(top, y); output(top, x)"
          [42]
          "high"
          [] -- x should be erased because it depends on y
          True,
        runObserverTest
          "Deep Erasure: control flow (x depends on y via PC)"
          DynamicNSU
          "input(high, y); x := y; if y then x := 1 else skip; erase(top, y); output(top, x)"
          [1]
          "high"
          [] -- x should be erased because it was assigned under a PC influenced by y
          True,
        runObserverTest
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
        runObserverTest
          "Deep Erasure: transitivity (z depends on x depends on y)"
          DynamicNSU
          "input(high, y); x := y; z := x; erase(top, y); output(top, z)"
          [42]
          "high"
          [] -- z should be erased
          True,
        -- MORE EDGE CASES
        runObserverTest
          "Deep Erasure: circular dependency (x:=y; y:=x; erase y)"
          DynamicNSU
          "input(high, y); x := y; y := x; erase(top, y); output(top, x)"
          [42]
          "high"
          [] -- x should be erased because it transitively depends on y
          True,
        runObserverTest
          "Deep Erasure: multiple influences (x:=y+z; erase y)"
          DynamicNSU
          "input(high, y); input(high, z); x := y + z; erase(top, y); output(top, x)"
          [10, 20]
          "high"
          [] -- x should be erased because it depends on y (even though z is fine)
          True,
        runObserverTest
          "Deep Erasure: erasure inside function affects caller result"
          DynamicNSU
          "def f(a) { erase(top, a) } return a; input(high, y); x := call f(y); output(top, x)"
          [42]
          "high"
          [] -- x should be 0 because 'a' was erased inside the function before return
          True,
        runObserverTest
          "Deep Erasure: while loop dependency (x incremented based on y)"
          DynamicNSU
          "input(high, y); x := y; while y do (x := x + 1; y := y - 1); erase(top, y); output(top, x)"
          [3]
          "high"
          [] -- x was modified under PC influenced by y, so it depends on y
          True,
        runObserverTest
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
        runObserverTest
          "Deep Erasure: prevent erasure after influencing variable overwrite"
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
        runObserverTest
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
        runObserverTest
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

        runObserverTest
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

        runObserverTest
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
        -- ERASE l_target_v JOIN: counter-examples for the formula in
        -- `stepErase`. The label assigned to each dependent v is
        --   l_target_v = l_cmd ⊔ Γ(v) ⊔ pc
        -- Dropping the Γ(v) (and pc) terms — i.e. setting l_target_v = l_cmd
        -- — silently *lowers* a dependent's label below its true
        -- sensitivity. The high-view value is preserved by
        -- `eraseMultiMemory` (it only zeros views ⋡ l_target_v), so the
        -- dependent still carries secret-influenced data while claiming
        -- to live at a low level. The tests below all PASS under the
        -- current (correct) formula; flipping line 445 of Imp.hs to
        -- `let l_target_v = l_cmd` makes the first one fail, exhibiting
        -- the leak.
        ---------------------------------------------------------------------

        -- Unconditional shape. Buggy formula → labs x = low after the
        -- erase, so output(low, x) is wrongly accepted and emits the
        -- (zero) low-view of x. Correct formula → labs x = high ⊔ low ⊔
        -- bot = high, output(low, x) is rejected.
        runTest
          "Erase l_target join: dependent's label must not drop (unconditional)"
          DynamicNSU
          "input(high, s); x := s; erase(low, s); output(low, x)"
          [42]
          ShouldFail
          True,
        -- Conditional shape — the actual non-interference violation.
        -- With the buggy formula and c=1, the erase fires under pc=high
        -- and demotes labs x to low; the subsequent output(low, x)
        -- completes (emitting 0). With c=0 the erase is skipped, labs x
        -- stays high, and output(low, x) aborts. So under the buggy
        -- formula the low observer distinguishes the secret c by abort-
        -- vs-completion. Under the correct formula both inputs abort
        -- uniformly (labs x ends up at high either way).
        runTest
          "Erase l_target join: conditional erase under high pc — buggy formula leaks (c=1)"
          DynamicNSU
          "input(high, c); x := c; if c then erase(low, c) else skip; output(low, x)"
          [1]
          ShouldFail
          True,
        runTest
          "Erase l_target join: same program with c=0 (the matching half of the pair)"
          DynamicNSU
          "input(high, c); x := c; if c then erase(low, c) else skip; output(low, x)"
          [0]
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
        runObserverTest
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
        runObserverTest
          "y independent of x when f returns constant"
          DynamicNSU
          "def f(a) { skip } return 42; input(low, x); y := call f(x); erase(top, x); output(low, y)"
          [10]
          "low"
          [42]
          True,
        -- Self-call x := call f(x): the new x reaches resolved through
        -- the args, so caller's old x-deps must be preserved.
        runObserverTest
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
        -- are initialized to `pc` rather than ⊥.
        ---------------------------------------------------------------------

        -- The call is under high pc and the result is assigned to a target
        -- whose label is already high, so the return-side NSU also passes.
        runObserverTest
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
        runObserverTest
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
        runObserverTest
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
        runObserverTest
          "PU permissive at function return"
          DynamicPU
          "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(high, x)"
          [1]
          "high"
          [0]
          True,
        ---------------------------------------------------------------------
        -- carriers vs newXDeps at function return. Both lines in stepReturn
        -- compute `Set.union resolved callerPcVars`, but they feed different
        -- consumers:
        --   * `carriers` (line 530) → applyNsu's pFromRhs check.
        --   * `newXDeps`  (line 536) → caller's influence map.
        --
        -- The union is *redundant for carriers*: for any X ∈ callerPcVars,
        -- either X ∈ resolved (X is not a callee body-local, so resolve
        -- keeps it) or X ∈ calleeIntro (name collision) which means
        -- cleanedP = cfgPartials \ calleeIntro has already removed X. So
        -- pFromRhs is identical with or without the union. No test in PU
        -- can distinguish the two — that's why dropping callerPcVars from
        -- `carriers` keeps the entire suite green.
        --
        -- The union is *not redundant for newXDeps*: the influence map has
        -- no calleeIntro filter. A name-collision callerPcVar is dropped
        -- by `resolve` but is genuinely a dependency of y, and a later
        -- erase on it must still deep-erase y. The test below exercises
        -- precisely this case.
        ---------------------------------------------------------------------

        -- Regression for `carriers`'s only role: PU's pFromRhs check at
        -- return. v is P-marked, passed as an argument, and the callee
        -- returns the parameter. Via argToVars, v lands in `resolved`,
        -- so y inherits P and the branch on y aborts. Passes whether
        -- `carriers = resolved ∪ callerPcVars` or just `resolved` (by
        -- the proof above — the union with callerPcVars is provably
        -- redundant on line 530).
        runTest
          "PU return: P propagates from caller-marked arg via resolved"
          DynamicPU
          "def f(a) { skip } return a; input(high, s); v := 0; if s then v := 1 else skip; y := call f(v); if y then output(high, 1) else output(high, 0)"
          [1]
          ShouldFail
          True,
        -- Counter-example for the SAME-LOOKING union on line 536
        -- (`newXDeps`). Callee body re-uses the caller's pc-var name `c`,
        -- so `resolve` drops it. Without the union, y's influence set
        -- never records the c→y edge, and a subsequent erase(top, c)
        -- fails to deep-erase y; labs y stays at `high` instead of
        -- rising to `top`, and output(high, y) is wrongly accepted.
        -- Run under DynamicPU so the high-pc return is allowed (it would
        -- NSU-abort under DynamicNSU before we ever reach the erase).
        runTest
          "newXDeps union: name-collision callerPcVar must reach y's deps"
          DynamicPU
          "def f() { c := 5 } return 0; input(high, c); if c then y := call f() else skip; erase(top, c); output(high, y)"
          [1]
          ShouldFail
          True,
        ---------------------------------------------------------------------
        -- M-Erase-PU: the P' propagation rule
        --   P' = P ∪ dep(x, I)  if upgrade(x) or x ∈ P
        --      = P              otherwise
        -- has two distinct triggers and these tests isolate each in turn.
        -- A static check would reject both programs; under DynamicPU they
        -- run far enough to exercise the rule, and the deferred PU branch
        -- check fires precisely when P propagation works as intended.
        ---------------------------------------------------------------------

        -- (1) upgrade(x) case. erase(low, x) runs with pc = high and
        -- labs x = ⊥, so nsuFail (i.e. upgrade(x)) is true. The rule
        -- must add dep(x, I) = {x, y} to P, not just x. Without the
        -- dependent y entering P, the later `if y` branch slips past
        -- the PU check and the program completes. With the rule, the
        -- branch on y aborts.
        runTest
          "M-Erase-PU: upgrade(x) propagates P to dep(x, I), branch on y aborts"
          DynamicPU
          "input(high, c); x := 5; y := x; if c then erase(low, x) else skip; if y then output(top, 1) else output(top, 0)"
          [1]
          ShouldFail
          True,
        -- (2) x ∈ P case (with no upgrade at the erase itself). The
        -- reflexive `x := x + 1` PU-upgrades x at the assignment, but
        -- because x ∈ closure({x}) the influence-map cleanup is
        -- skipped — y's dependency on x survives. Then erase(low, x)
        -- runs under pc = ⊥ ⊑ labs x = high (NSU passes, no upgrade),
        -- so propagation is purely via the `x ∈ P` arm of the rule.
        -- Without it, y stays out of P and `if y` runs.
        runTest
          "M-Erase-PU: x ∈ P (no upgrade at erase) still propagates to dep(x, I)"
          DynamicPU
          "input(high, s); x := 5; y := x; if s then x := x + 1 else skip; erase(low, x); if y then output(high, 1) else output(high, 0)"
          [1]
          ShouldFail
          True,
        -- (3) Negative companion: when neither upgrade(x) nor x ∈ P
        -- holds, the rule's `otherwise` arm leaves P unchanged.
        -- Here labs x = high already (from x := s), pc = ⊥ at the
        -- erase, no prior PU marks. The branch on y must NOT abort.
        runObserverTest
          "M-Erase-PU: neither trigger fires — P stays empty, branch on y runs"
          DynamicPU
          "input(high, s); x := s; y := x; erase(low, x); if y then output(high, 1) else output(high, 0)"
          [42]
          "high"
          [1]
          True,
        ---------------------------------------------------------------------
        -- OBSERVER VIEW. cfgOutput stores one (channel, value) entry per
        -- emission, tagged with the level named in `output(...)`. An
        -- observer at level L sees every entry whose channel ch ⊑ L
        -- (same filter as printSecurityReport in Examples.hs).
        -- output(high, 42) puts ONE entry (high, 42) on the tape; the
        -- high and top observers both see it (high ⊑ high ⊑ top), while
        -- the low and bottom observers see nothing (high ⋢ low/bottom).
        ---------------------------------------------------------------------

        runObserverTest
          "Observer view: high observer sees output(high, 42)"
          DynamicNSU
          "output(high, 42)"
          []
          "high"
          [42]
          True,
        runObserverTest
          "Observer view: top observer also sees output(high, 42) because high ⊑ top"
          DynamicNSU
          "output(high, 42)"
          []
          "top"
          [42]
          True,
        runObserverTest
          "Observer view: low observer sees nothing (high ⋢ low)"
          DynamicNSU
          "output(high, 42)"
          []
          "low"
          []
          True,
        -- Multiple outputs at mixed levels: the top observer sees them all
        -- (every channel flows to top). The low observer sees only low ones.
        runObserverTest
          "Observer view: top accumulates outputs from every channel ≤ top"
          DynamicNSU
          "output(low, 1); output(high, 2); output(top, 3)"
          []
          "top"
          [1, 2, 3]
          True,
        runObserverTest
          "Observer view: low only sees outputs to low (or bottom)"
          DynamicNSU
          "output(low, 1); output(high, 2); output(top, 3)"
          []
          "low"
          [1]
          True,
        ---------------------------------------------------------------------
        -- INFLUENCE-MAP CLEANUP IN CONDITIONAL CONTEXT.
        -- pc_vars must be carried into the new dep set, even when cleaning.
        ---------------------------------------------------------------------

        -- Under if c then x := ..., the new x's deps must include c (via
        -- pc_vars). erase(top, c) should then also erase x.
        runObserverTest
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
          ShouldFail,
        staticTest
          "Top-vs-bot fallback: bot accepts a soundness gap (g indirectly calls leaky f)"
          "def f() { input(high, s); output(low, s) } return 0; def g() { z := call f(); output(high, z) } return z; y := call g(); output(high, y)"
          ShouldFail,
        staticTest
          "Top-vs-bot fallback: bot accepts a dead-branch reference to leaky f"
          "def f() { input(high, s); output(low, s) } return 0; def g() { if 0 then r := call f() else r := 5; output(high, r) } return r; y := call g(); output(high, y)"
          ShouldFail,
        staticTest
          "Top-vs-bot fallback: bot accepts a dead-branch reference to leaky f"
          "def f() { input(high, s); output(low, s) } return 0; def g() { r := 5; output(high, r) } return r; y := call g(); output(high, y)"
          ShouldPass,
        runTest
          "testing"
          DynamicPU
          "input(bottom, x); input(low, x); input(high, x); input(top, x); erase(low, x)"
          [1, 2, 3, 4]
          ShouldPass
          True
      ]

  let passed = length (filter id results)
      total = length results
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  let summaryColor = if passed == total then boldGreen else boldRed
  putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed")
  putStrLn (bold (replicate 60 '='))
