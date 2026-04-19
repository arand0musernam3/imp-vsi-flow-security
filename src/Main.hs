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
  let progStr5 = "input(secret, x); output(public, x)" -- Should be rejected
  let progStr6 = "input(secret, x); x := 42 ; z := 3 ;  output(public, x)" -- Should be accepted
  
  let tests = [progStr1, progStr2, progStr3, progStr4, progStr5, progStr6]
  mapM_ (\progStr -> do
    putStrLn $ "Testing: " ++ progStr
    runStringTyped progStr mZ
    putStrLn ""
    ) tests

  -- Input with explicit tape --
  let progWithInput = "input(public, a); input(secret, b); c := a + b; output(secret, c)"
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
