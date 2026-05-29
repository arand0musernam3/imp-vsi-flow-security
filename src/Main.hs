module Main where

import qualified RegressionTests
import System.Environment (getArgs)
import System.Exit (exitFailure)
import TestUtils

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> reportExamples
    ["--tests"] -> RegressionTests.runAllRegressionTests
    _ -> do
      putStrLn "Usage: imp-vsi-type-system [--tests]"
      exitFailure

reportExamples :: IO ()
reportExamples = do
  putStrLn ""
  putStrLn (bold (replicate 70 '='))
  putStrLn (bold "  Static vs Dynamic NSU vs Dynamic PU — examples from the report")
  putStrLn (bold (replicate 70 '='))

  putStrLn ""
  putStrLn (boldYellow "### Worked examples (report §Comparison › Worked examples) ###")

  worked <-
    sequence
      [ compareModes
          "Listing 5a"
          "implicit flow, output(low, x) — all three styles reject/abort"
          prog1a
          [("high", 42)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Listing 5b"
          "implicit flow, output(high, x) — NSU over-rejects, static and PU accept"
          prog1b
          [("high", 42)]
          (Accept "", Abort "", Accept ""),
        compareModes
          "Listing 6"
          "erase under high pc, output to high — PU win on erase"
          prog5
          [("high", 0), ("low", 7)]
          (Accept "", Abort "", Accept ""),
        compareModes
          "Listing 7"
          "deferred PU check fires at a later branch on a P-marked variable"
          prog10
          [("high", 1)]
          (Accept "", Abort "", Abort ""),
        compareModes
          "Listing 8"
          "function leaks a high argument to a low channel — all three reject"
          prog2
          [("high", 42)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Listing 9"
          "nested function call with a deep direct leak — all three reject"
          prog13
          [("high", 42)]
          (Reject "", Abort "", Abort "")
      ]

  putStrLn ""
  putStrLn (boldYellow "### Exercises (report §Comparison › Exercises for the reader) ###")

  exercises <-
    sequence
      [ compareModes
          "Listing 10"
          "incomparable lattice — direct flow between L1 and L2 is rejected"
          prog3
          [("L1", 7)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Listing 11"
          "overwrite kills label dependence — all three accept"
          prog4
          [("high", 99)]
          (Accept "", Accept "", Accept ""),
        compareModes
          "Listing 12"
          "snapshot v taken before re-input and erase — all three accept"
          prog7
          [("high", 1), ("high", 8)]
          (Accept "", Accept "", Accept ""),
        compareModes
          "Listing 13"
          "while-loop with secret guard — NSU aborts at first body write, PU accepts"
          prog9
          [("high", 3)]
          (Accept "", Abort "", Accept ""),
        compareModes
          "Listing 10"
          "function called inside a high branch, high target — all three accept"
          prog8
          [("high", 5)]
          (Accept "", Accept "", Accept ""),
        compareModes
          "Listing 15"
          "function return under high pc — NSU aborts at return-assign, PU accepts"
          prog11
          [("high", 1)]
          (Accept "", Abort "", Accept ""),
        compareModes
          "Listing 16"
          "erase after function leaves c at top — output(high) fails"
          prog12
          [("high", 1)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Listing 17"
          "PU check on output — NSU over-rejects, static and PU accept"
          "input(high, s); input(bottom, x); y:=x; if s then erase(high, x) else skip; if y then output(bottom, 1) else output(bottom, 0)"
          [ ("high", 1), ("bottom", 4)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Test"
          "Confidentiality and integrity test"
          prog14
          [ ("ST", 2)]
          (Accept "", Accept "", Accept ""),
        compareModes
          "Test1"
          "Confidentiality and integrity test1"
          prog15
          [ ("ST", 1), ("PU",2)]
          (Reject "", Abort "", Abort ""),
        compareModes
          "Test2"
          "Confidentiality and integrity test2"
          prog16
          [ ("ST", 1), ("PU",2)]
          (Reject "", Abort "", Abort "")
      ]

  let results = worked ++ exercises
      passed = length (filter id results)
      total = length results
  putStrLn ""
  putStrLn (bold (replicate 70 '='))
  let summaryColor = if passed == total then boldGreen else boldRed
  putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " examples matched their prediction")
  putStrLn (bold (replicate 70 '='))
  where
    prog1a =
      "input(high, secret); x := 0; "
        ++ "if secret then x := 1 else skip; "
        ++ "output(low, x)"
    prog1b =
      "input(high, secret); x := 0; "
        ++ "if secret then x := 1 else skip; "
        ++ "output(high, x)"
    prog2 =
      "def f(a) { output(low, a) } return 0; "
        ++ "input(high, secret); x := call f(secret)"
    prog3 =
      "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; "
        ++ "input(L1, x); output(L2, x)"
    prog4 =
      "input(high, y); z := y + 1; x := z; x := 7; output(bottom, x)"
    prog5 =
      "input(high, y); input(low, z); "
        ++ "if y then skip else erase(high, z); "
        ++ "output(high, z)"
    prog7 =
      "input(high, c); x := 5; v := x; input(high, x); "
        ++ "if c then erase(top, x) else skip; "
        ++ "output(low, v)"
    prog8 =
      "def f(a) { t := a; t := t + 1 } return t; "
        ++ "input(high, s); y := s; "
        ++ "if s then y := call f(s) else skip; "
        ++ "output(high, y)"
    prog9 =
      "input(high, c); x := 0; y := 0; "
        ++ "while c do (x := x + 1; y := y + x; c := c - 1); "
        ++ "output(high, y); output(high, x)"
    prog10 =
      "input(high, secret); x := 0; "
        ++ "if secret then x := 1 else skip; "
        ++ "if x then output(high, 1) else output(high, 0)"
    prog11 =
      "def f(a) { skip } return a; "
        ++ "input(high, secret); x := 0; "
        ++ "if secret then x := call f(0) else skip; "
        ++ "output(high, x)"
    prog12 =
      "def f() { c := 5 } return 0; "
        ++ "input(high, c); "
        ++ "if c then y := call f() else skip; "
        ++ "erase(top, c); output(high, y)"
    prog13 =
      "def f() { input(high, s); output(low, s) } return 0; "
        ++ "def g() { z := call f(); output(top, z) } return z; "
        ++ "y := call g(); output(top, y)"
    prog14 =
      "lattice { PT < PU, PT < ST, PU < SU, ST < SU };"
      ++ "input(ST, password); data := password; output(SU, data)"
    prog15 =
      "lattice { PT < PU, PT < ST, PU < SU, ST < SU };"
      ++ "input(PU, flag); input(ST, db_state); if flag then db_state := 1 + db_state else skip; output(PU, db_state)"
    prog16 =
      "lattice { PT < PU, PT < ST, PU < SU, ST < SU };"
      ++ "input(PU, flag); input(ST, db_state); if flag then db_state := 1 + db_state else skip; output(ST, db_state)"