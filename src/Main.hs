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

------------------------------------------------------------------
-- Report examples: each compareModes call corresponds to a named
-- subsection (E1, E2, ...) in docs/report/main.typ. The third tuple
-- argument is the predicted (Static, NSU, PU) outcome — printed and
-- matched at run time, so the report's claims are verified.
------------------------------------------------------------------

reportExamples :: IO ()
reportExamples = do
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  putStrLn (bold "  Static vs Dynamic — worked examples (report)")
  putStrLn (bold (replicate 60 '='))

  oks <-
    sequence
      [ -- E1: Static can't see that the then-branch is dead code. A
        -- compiler with dead-code elimination would rewrite the program
        -- to `input(high, s); skip; output(low, 1)` and all three modes
        -- would agree; the runtime monitors already act as if DCE had
        -- happened.
        compareModes
          "E1"
          "Dead branch with leak — static over-approximates"
          "input(high, s); if 0 then output(low, s) else skip; output(low, 1)"
          [42]
          (Reject "", Accept "", Accept ""),
        -- E2: Static joins both branches' post-environments, so x is
        -- always treated as high after the if. Dynamic is path-sensitive
        -- per run.
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
        -- E3: Canonical PU example. Static accepts (x becomes high after
        -- the join; output(high, x) passes). NSU aborts at the upgrade.
        -- PU defers, runs the upgrade, output is fine because the channel
        -- dominates the upgraded label.
        compareModes
          "E3"
          "Canonical PU win — upgrade only used at a dominating channel (s=1)"
          "input(high, s); x := 0; if s then x := 1 else skip; output(high, x)"
          [1]
          (Accept "", Abort "", Accept ""),
        -- E4: PU's deferred abort. Same upgrade as E3, but the upgraded
        -- variable is then used as a branch condition — PU's P-check
        -- fires. Note: static *accepts* this entire program, but PU
        -- *rejects* this specific run (s=1). Static is flow-insensitive
        -- at branch conditions; PU's P-set is path-sensitive.
        compareModes
          "E4"
          "PU's deferred abort — branch on P-marked variable (s=1)"
          "input(high, s); x := 0; if s then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [1]
          (Accept "", Abort "", Abort ""),
        -- E5: Static rejects the program because the fixed-point body
        -- type makes x high after the loop. Dynamic is happy when the
        -- loop body never executes.
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
        -- E6: Direct flow leak. All three agree.
        compareModes
          "E6"
          "Direct leak — all three reject"
          "input(high, s); output(low, s)"
          [42]
          (Reject "", Abort "", Abort ""),
        -- E7: Input side-channel under a high pc. PU does *not* relax
        -- the channel check, so it aborts on the same paths as NSU.
        compareModes
          "E7"
          "Input under high pc — channel check is not relaxed by PU"
          "input(high, s); if s then input(low, x) else skip"
          [1]
          (Reject "", Abort "", Abort ""),
        -- E8: Function call whose body would leak on some argument level
        -- combination. Static rejects the program (the (leak, [high], bot)
        -- summary key was dropped in Phase 2 of computeSummaries; the call
        -- site references it). Dynamic only sees the leak on inputs that
        -- actually reach the bad path.
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
