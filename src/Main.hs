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
    runStringTyped progStr mZ
    putStrLn ""
    ) tests

  -- Input with explicit tape --
  let progWithInput = "input(low, a); input(high, b); c := a + b; output(high, c)"
  let inputTape = [10, 20]
  putStrLn $ "Code: " ++ progWithInput
  putStrLn $ "Input Tape: " ++ show inputTape
  runStringTypedWithInput progWithInput mZ inputTape

  -- PDF Examples from assignment 6 --
  putStrLn "\n--- PDF Examples ---"
  putStrLn $ "Code: " ++ pdfExample1
  runStringTyped pdfExample1 mZ

  putStrLn ""
  putStrLn $ "Code: " ++ pdfExample2
  runStringTyped pdfExample2 mZ

  -- Function Tests --
  putStrLn "\n--- FUNCTION TESTS ---"
  
  let funcTest1 = "def inc(a) { a := a + 1 } return a; x_p := 5; y_p := call inc(x_p)"
  putStrLn $ "Testing: " ++ funcTest1
  runStringTyped funcTest1 mZ

  let funcTest2 = "def f(s) { if s then r := 1 else r := 0 } return r; x_s := 1; y_p := call f(x_s); output(low, y_p)"
  putStrLn $ "\nTesting (Should pass): " ++ funcTest2
  runStringTyped funcTest2 mZ

  let funcTest3 = "def f(s) { if s then r := 1 else r := 0 } return r; input(high, x); y_p := call f(x); output(low, y_p)"
  putStrLn $ "\nTesting (Should pass): " ++ funcTest3
  runStringTyped funcTest3 mZ

  let funcTest4 = "def f(s) { r := s } return r; x_s := 42; y_p := call f(x_s); output(high, y_p)"
  putStrLn $ "\nTesting (Should pass): " ++ funcTest4
  runStringTyped funcTest4 mZ

  -- Diamond Lattice Test --
  putStrLn "\n--- DIAMOND LATTICE TEST ---"
  -- L1 and L2 are incomparable.
  -- input(L1) + input(L2) -> output(High) should pass.
  -- input(L1) -> output(L2) should fail.
  let diamondLattice = "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; "
  let diamondTest1 = diamondLattice ++ "input(L1, x); input(L2, y); z := x + y; output(High, z)"
  let diamondTest2 = diamondLattice ++ "input(L1, x); output(L2, x)"
  
  putStrLn $ "Testing Diamond (Should pass): " ++ diamondTest1
  runStringTyped diamondTest1 mZ
  
  putStrLn $ "\nTesting Diamond (Should fail): " ++ diamondTest2
  runStringTyped diamondTest2 mZ
