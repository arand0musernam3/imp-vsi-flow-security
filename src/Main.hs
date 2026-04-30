module Main where

import Examples
import Parser (parseImp)
import Types

main :: IO ()
main = do
  -- Run tests on example programs --
  putStrLn "--- TESTS ---"
  let progStr1 = "y_s := 42; x_p := y_s"
  let progStr2 = "if y_s then x_p := 1 else x_p := 0"
  let progStr3 = "while y_s do skip"
  let progStr4 = "if y_s then { x_p := 1; x_p := 0 } else skip"
  let progStr5 = "input(high, x); output(low, x)" -- Should be rejected
  let progStr6 = "input(high, x); x := 42 ; z := 3 ;  output(low, x)" -- Should be accepted
  
  let tests = [progStr1, progStr2, progStr3, progStr4, progStr5, progStr6]
  mapM_ (\progStr -> do
    putStrLn $ "Testing: " ++ progStr
    runStringTyped progStr
    putStrLn ""
    ) tests

  -- Input with explicit tape --
  let progWithInput = "input(low, a); input(high, b); c := a + b; output(high, c)"
  let inputTape = [10, 20]
  putStrLn $ "Code: " ++ progWithInput
  putStrLn $ "Input Tape: " ++ show inputTape
  runStringTypedWithInput progWithInput inputTape

  -- PDF Examples from assignment 6 --
  putStrLn "\n--- PDF Examples ---"
  putStrLn $ "Code: " ++ pdfExample1
  runStringTyped pdfExample1

  putStrLn ""
  putStrLn $ "Code: " ++ pdfExample2
  runStringTyped pdfExample2

  -- Function Tests --
  putStrLn "\n--- FUNCTION TESTS ---"
  
  let funcTest1 = "def inc(a) { a := a + 1 } return a; x_p := 5; y_p := call inc(x_p)"
  putStrLn $ "Testing: " ++ funcTest1
  runStringTyped funcTest1

  let funcTest2 = "def f(s) { if s then r := 1 else r := 0 } return r; x_s := 1; y_p := call f(x_s); output(low, y_p)"
  putStrLn $ "\nTesting (Should pass): " ++ funcTest2
  runStringTyped funcTest2

  let funcTest3 = "def f(s) { if s then r := 1 else r := 0 } return r; input(high, x); y_p := call f(x); output(low, y_p)"
  putStrLn $ "\nTesting (Should fail static, run dynamic): " ++ funcTest3
  runStringTyped funcTest3

  -- Multi-level Erasure Tests --
  putStrLn "\n--- MULTI-LEVEL ERASURE TESTS ---"

  let eraseTest1 = "x := 42; erase(high, x)"
  putStrLn $ "Testing visibility after erase(high, x): " ++ eraseTest1
  runStringTyped eraseTest1

  let eraseTest2 = "input(high, x); y := x; erase(high, y)"
  putStrLn $ "\nTesting erasure of high input: " ++ eraseTest2
  runStringTypedWithInput eraseTest2 [55]

  let eraseTest3 = "x := 10; y_s := 1; if y_s then erase(high, x) else skip"
  putStrLn $ "\nTesting erasure under secret PC: " ++ eraseTest3
  runStringTyped eraseTest3

  let eraseDiamond = "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; x := 100; erase(L1, x)"
  putStrLn $ "\nTesting diamond erasure (x=100 visible at L1 and High, 0 at Low and L2): " ++ eraseDiamond
  runStringTyped eraseDiamond
