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
  putStrLn (bold (replicate 60 '='))
  putStrLn (bold "  Static vs Dynamic — worked examples (report)")
  putStrLn (bold (replicate 60 '='))

  oks <-
    sequence
      [
        compareModes
          "E1"
          "Dead branch with leak — static over-approximates"
          "input(high, s); if 0 then output(low, s) else skip; output(low, 1)"
          [42]
          (Reject "", Accept "", Accept ""),
        compareModes
          "E2a"
          "Branch on secret; safe input (s=0)"
          "input(high, s); x := 7; if s then x := 7 else skip; output(low, x)"
          [0]
          (Reject "", Accept "", Accept ""),
        compareModes
          "E2b"
          "Branch on secret; unsafe input (s=1) — NSU aborts at assign, PU at output"
          "input(high, s); x := 7; if s then x := 7 else skip; output(low, x)"
          [1]
          (Reject "", Abort "", Abort ""),
        compareModes
          "E3"
          "Canonical PU win — upgrade only used at a dominating channel (s=1)"
          "input(high, s); x := 0; if s then x := 1 else skip; output(high, x)"
          [1]
          (Accept "", Abort "", Accept ""),
        compareModes
          "E4"
          "PU's deferred abort — branch on P-marked variable (s=1)"
          "input(high, s); x := 0; if s then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [1]
          (Accept "", Abort "", Abort ""),
        compareModes
          "E5a"
          "Loop with secret condition — c=0 never enters the body"
          "input(high, c); x := 5; while c do (x := x + 1; c := c - 1); output(low, x)"
          [0]
          (Reject "", Accept "", Accept ""),
        compareModes
          "E5b"
          "Loop with secret condition — c=3 actually iterates"
          "input(high, c); x := 5; while c do (x := x + 1; c := c - 1); output(low, x)"
          [3]
          (Reject "", Abort "", Abort ""),
        compareModes
          "E6"
          "Direct leak — all three reject"
          "input(high, s); output(low, s)"
          [42]
          (Reject "", Abort "", Abort ""),
        compareModes
          "E7"
          "Input under high pc — channel check is not relaxed by PU"
          "input(high, s); if s then input(low, x) else skip"
          [1]
          (Reject "", Abort "", Abort ""),
        compareModes
          "E8a"
          "Function call — leaky path unreached at runtime (s=0)"
          "def leak(a) { if a then output(low, a) else skip } return 0; input(high, s); x := call leak(s)"
          [0]
          (Reject "", Accept "", Accept ""),
        compareModes
          "E8b"
          "Function call — leaky path reached at runtime (s=1)"
          "def leak(a) { if a then output(low, a) else skip } return 0; input(high, s); x := call leak(s)"
          [1]
          (Reject "", Abort "", Abort "")
      ]

  let passed = length (filter id oks)
      total = length oks
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  let summaryColor = if passed == total then boldGreen else boldRed
  putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " examples matched their prediction")
  putStrLn (bold (replicate 60 '='))
