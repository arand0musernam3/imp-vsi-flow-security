module RegressionTests (runAllRegressionTests) where

import Imp
import TestUtils

runAllRegressionTests :: IO ()
runAllRegressionTests = do
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
          [("high", 1)]
          ShouldFail
          True,
        runTest
          "NSU on assignment (branch not taken, secret=0)"
          DynamicNSU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
          [("high", 0)]
          ShouldPass
          True,
        runTest
          "Explicit flow on output (secret to low channel)"
          DynamicNSU
          "input(high, secret); output(low, secret)"
          [("high", 42)]
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
          [("high", 1), ("low", 99)]
          ShouldFail
          True,
        runTest
          "NSU on function return (high pc, public target)"
          DynamicNSU
          "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(low, x)"
          [("high", 1)]
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
          [("high", 1)]
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
          [("high", 42)]
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
          [("high", 42), ("low", 99)]
          ShouldPass
          True,
        runObserverTest
          "Call: high-labeled arg reaches callee's high view"
          DynamicNSU
          "def f(a) { skip } return a; input(high, secret); x := call f(secret); output(high, x)"
          [("high", 42)]
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
        staticTest
          "Static rejects call whose body fails for these arg levels"
          "def f(a) { output(low, a) } return 0; input(high, secret); x := call f(secret)"
          ShouldFail,
        staticTest
          "Static accepts call when body type-checks for these arg levels"
          "def f(a) { skip } return a; x := 7; y := call f(x)"
          ShouldPass,
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
          [("L1", 3), ("L2", 4)]
          ShouldPass
          True,
        runTest
          "Run custom lattice program - diamond lattice"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          [("L1", 3), ("L2", 4)]
          ShouldFail
          True,

        runObserverTest
          "Label reset: overwrite with constant clears high label"
          DynamicNSU
          "input(high, y); z := y + 1; x := z; x := 7; output(bottom, x)"
          [("high", 42)]
          "bottom"
          [7]
          True,
        runTest
          "Label reset: NSU fires after overwrite resets label"
          DynamicNSU
          "input(high, y); x := y; x := 7; if y then x := 3 else skip"
          [("high", 1)]
          ShouldFail
          True,
        runObserverTest
          "Diamond: label reset on overwrite allows output to Low"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; output(Low, x)"
          [("L1", 42)]
          "Low"
          [5]
          True,
        runTest
          "Diamond: NSU at incomparable level is always caught"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, z); if z then x := 5 else skip"
          [("L1", 42), ("L2", 1)]
          ShouldFail
          True,
        runTest
          "Diamond: overwrite resets label so NSU fires under same-level PC"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); x := 5; input(L1, z); if z then x := 3 else skip"
          [("L1", 42), ("L1", 1)]
          ShouldFail
          True,
        runObserverTest
          "Diamond: L2 overwrite after L1 data, output to L2"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); input(L2, y); x := y; output(L2, x)"
          [("L1", 42), ("L2", 7)]
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
          [("high", 1), ("low", 42)]
          "low"
          [42]
          True,
        runTest
          "Leaky conditional erasure - vulnerable dynamic path"
          DynamicNSU
          "input(high, y); input(low, z); if y then skip else erase(high, z); output(low, z)"
          [("high", 0), ("low", 42)]
          ShouldFail
          True,
        runObserverTest
          "Deep Erasure: data flow (x depends on y)"
          DynamicNSU
          "input(high, y); x := y; erase(top, y); output(top, x)"
          [("high", 42)]
          "high"
          [] -- x should be erased because it depends on y
          True,
        runObserverTest
          "Deep Erasure: control flow (x depends on y via PC)"
          DynamicNSU
          "input(high, y); x := y; if y then x := 1 else skip; erase(top, y); output(top, x)"
          [("high", 1)]
          "high"
          [] -- x should be erased, because it was assigned under a PC influenced by y
          True,
        runObserverTest
          "Deep Erasure: dependency removal on overwrite"
          DynamicNSU
          "input(high, y); x := y; x := 7; erase(high, y); output(bottom, x)"
          [("high", 42)]
          "bottom"
          [7] -- x should not be erased because it was overwritten
          True,
        staticTest
          "Deep Erasure: static rejection of leak after erasure"
          "input(high, y); x := y; erase(high, y); output(low, x)"
          ShouldFail, -- x is raised to high by deep erasure, so output(low) fails
        runObserverTest
          "Deep Erasure: transitivity (z depends on x depends on y)"
          DynamicNSU
          "input(high, y); x := y; z := x; erase(top, y); output(top, z)"
          [("high", 42)]
          "high"
          [] -- z should be erased
          True,
        -- MORE EDGE CASES
        runObserverTest
          "Deep Erasure: circular dependency (x:=y; y:=x; erase y)"
          DynamicNSU
          "input(high, y); x := y; y := x; erase(top, y); output(top, x)"
          [("high", 42)]
          "high"
          [] -- x should be erased because it transitively depends on y
          True,
        runObserverTest
          "Deep Erasure: multiple influences (x:=y+z; erase y)"
          DynamicNSU
          "input(high, y); input(high, z); x := y + z; erase(top, y); output(top, x)"
          [("high", 10), ("high", 20)]
          "high"
          [] -- x should be erased because it depends on y (even though z is fine)
          True,
        runObserverTest
          "Deep Erasure: erasure inside function affects caller result"
          DynamicNSU
          "def f(a) { erase(top, a) } return a; input(high, y); x := call f(y); output(top, x)"
          [("high", 42)]
          "high"
          [] -- x should be 0 because 'a' was erased inside the function before return
          True,
        runObserverTest
          "Deep Erasure: while loop dependency (x incremented based on y)"
          DynamicNSU
          "input(high, y); x := y; while y do (x := x + 1; y := y - 1); erase(top, y); output(top, x)"
          [("high", 3)]
          "high"
          [] -- x was modified under PC influenced by y, so it depends on y
          True,
        runObserverTest
          "Deep Erasure: conditional erasure (erase only if z is true)"
          DynamicNSU
          "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(high, x)"
          [("high", 42), ("low", 0)]
          "high"
          [42] -- z is false, so no erasure should happen
          True,
        runTest
          "Deep Erasure: conditional erasure (taken)"
          DynamicNSU
          "input(high, y); input(low, z); x := y; if z then erase(top, y) else skip; output(high, x)"
          [("high", 42), ("low", 1)]
          ShouldFail
          True,
        runObserverTest
          "Deep Erasure: prevent erasure after influencing variable overwrite"
          DynamicNSU
          "input(high, c); x:= 5; v := x; input(high, x); if c then erase(top, x) else skip; output(low, v)"
          [("high", 1), ("high", 10)]
          "low"
          [5]
          True,
        staticTest
          "While test"
          "input(high,c); x := 5; while c do x := x + 1"
          ShouldPass,
        runObserverTest
          "Infl cleanup: reflexive x := x+1 keeps a's dep on x"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(high, x); output(high, a)"
          []
          "high"
          [1]
          True,
        runTest
          "Infl cleanup: reflexive update + erase raises a to top (low output blocked)"
          DynamicNSU
          "x := 1; a := x; x := x + 1; erase(top, x); output(low, a)"
          []
          ShouldFail
          True,
        runObserverTest
          "Infl cleanup: non-reflexive `x := 5` breaks the x->a influence"
          DynamicNSU
          "x := 1; a := x; x := 5; erase(top, x); output(low, a)"
          []
          "low"
          [1]
          True,
        runTest
          "Infl cleanup: non-reflexive overwrite keeps a at high"
          DynamicNSU
          "input(high, secret); a := secret; secret := 0; output(low, a)"
          [("high", 42)]
          ShouldFail
          True,
        runObserverTest
          "Eager closure: erase propagates through a 4-step chain"
          DynamicNSU
          "input(high, x); y := x; z := y; w := z; erase(high, x); output(high, w)"
          [("high", 42)]
          "high"
          [42]
          True,
        runTest
          "Eager closure: every intermediate var rises to top"
          DynamicNSU
          "input(high, x); y := x; z := y; w := z; erase(top, x); output(high, w)"
          [("high", 42)]
          ShouldFail
          True,
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
        runObserverTest
          "Function locals at pc: body assigns under high caller pc"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(high, s); y := s; if s then y := call f(s) else skip; output(high, y)"
          [("high", 1)]
          "high"
          [2]
          True,
        runObserverTest
          "Function with two parameters"
          DynamicNSU
          "def f(a,b) { t := a + b } return t; a := 3; b := 4; y := call f(a, b); output(high, y)"
          []
          "high"
          [7]
          True,
        runTest
          "Function locals at pc: body assigns under high caller pc"
          DynamicNSU
          "def f(a) { t := a; t := t + 1 } return t; input(high, s); if s then y := call f(3) else skip; output(low, y)"
          [("high", 1)]
          ShouldFail
          True,
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
        runObserverTest
          "PU accepts where NSU rejects (output at high)"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(high, x)"
          [("high", 1)]
          "high"
          [1]
          True,
        runTest
          "PU still catches the leak via labels (output at low)"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(low, x)"
          [("high", 1)]
          ShouldFail
          True,
        runTest
          "PU rejects branch on P-marked variable"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [("high", 1)]
          ShouldFail
          True,
        runObserverTest
          "PU accepts the false-branch case symmetrically"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; output(high, x)"
          [("high", 0)]
          "high"
          [0]
          True,
        runTest
          "PU agrees with NSU on a direct flow leak"
          DynamicPU
          "input(high, secret); output(low, secret)"
          [("high", 42)]
          ShouldFail
          True,
        runObserverTest
          "PU permissive at function return"
          DynamicPU
          "def f(a) { skip } return a; input(high, secret); x := 0; if secret then x := call f(0) else skip; output(high, x)"
          [("high", 1)]
          "high"
          [0]
          True,
        runTest
          "PU return: P propagates from caller-marked arg via resolved"
          DynamicPU
          "def f(a) { skip } return a; input(high, s); v := 0; if s then v := 1 else skip; y := call f(v); if y then output(high, 1) else output(high, 0)"
          [("high", 1)]
          ShouldFail
          True,
        runTest
          "newXDeps union: name-collision callerPcVar must reach y's deps"
          DynamicPU
          "def f() { c := 5 } return 0; input(high, c); if c then y := call f() else skip; erase(top, c); output(high, y)"
          [("high", 1)]
          ShouldFail
          True,
        runTest
          "M-Erase-PU: upgrade(x) propagates P to dep(x, I), branch on y aborts"
          DynamicPU
          "input(high, c); x := 5; y := x; if c then erase(low, x) else skip; if y then output(top, 1) else output(top, 0)"
          [("high", 1)]
          ShouldFail
          True,
        runTest
          "M-Erase-PU: x ∈ P (no upgrade at erase) still propagates to dep(x, I)"
          DynamicPU
          "input(high, s); x := 5; y := x; if s then x := x + 1 else skip; erase(low, x); if y then output(high, 1) else output(high, 0)"
          [("high", 1)]
          ShouldFail
          True,
        runObserverTest
          "M-Erase-PU: neither trigger fires — P stays empty, branch on y runs"
          DynamicPU
          "input(high, s); x := s; y := x; erase(low, x); if y then output(high, 1) else output(high, 0)"
          [("high", 42)]
          "high"
          [1]
          True,
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
        runObserverTest
          "Implicit flow: erase(c) deep-erases x assigned under PC c"
          DynamicNSU
          "input(high, c); input(high, x); if c then x := x+1 else skip; erase(top, c); output(top, x)"
          [("high", 1), ("high", 1)]
          "top"
          [2]
          True,
        runTest
          "Implicit flow: x's label rises with c after deep erase"
          DynamicNSU
          "input(high, c); input(high, x); if c then x := x+1 else skip; erase(top, c); output(high, x)"
          [("high", 1), ("high", 1)]
          ShouldFail
          True,
        runTest
          "Implicit flow leak"
          DynamicNSU
          "input(high, x); y:=1; z:=1; if x then input(high, y) else skip; if y then z:=0 else skip; output(low, z)"
          [("high", 1), ("high", 0)]
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
          "def f() { input(high, s); output(low, s) } return 0; def g() { z := call f(); output(top, z) } return z; y := call g(); output(top, y)"
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
          "Channel-tagged tape: out-of-order inputs match by channel, not position"
          DynamicNSU
          "input(low, a); input(high, b); output(low, a); output(high, b)"
          [("high", 99), ("low", 7)]
          ShouldPass
          True,
        runTest
          "Channel-tagged tape: input on a channel with no matching entry aborts"
          DynamicNSU
          "input(low, a)"
          [("high", 99)]
          ShouldFail
          True,
        runTest
          "Channel-tagged tape: strict equality — bottom ≠ low even though bottom ⊑ low"
          DynamicNSU
          "input(low, a)"
          [("bottom", 7)]
          ShouldFail
          True,
        runTest
          "testing"
          DynamicPU
          "input(bottom, x); input(low, x); input(high, x); input(top, x); erase(low, x)"
          [("bottom", 1), ("low", 2), ("high", 3), ("top", 4)]
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
