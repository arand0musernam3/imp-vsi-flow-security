module Main where

import Examples
import Parser (parseImp)
import Types

main :: IO ()
main = do
  putStrLn "--- Testing Parser ---"
  let progStr1 = "y_s := 42; x_p := y_s"
  let progStr2 = "if y_s then x_p := 1 else x_p := 0"
  let progStr3 = "while y_s do skip"
  let progStr4 = "if y_s then { x_p := 1; x_p := 0 } else skip"
  let progStr5 = "input(secret, x); output(public, x)" -- Should be rejected by type system
  let progStr6 = "input(secret, x); x := 42 ; z := 3 ;  output(public, x)" -- Should be accepted by type system
  let tests = [progStr1, progStr2, progStr3, progStr4, progStr5, progStr6]
  mapM_ (\progStr -> do
    putStrLn $ "Testing: " ++ progStr
    case parseImp progStr of
      Left err -> print err
      Right p -> do
        putStrLn $ "Parsed: " ++ show p
        runTyped p mZ
    putStrLn ""
    ) tests

  putStrLn "\n--- PDF Example 1 (Rejected) ---"
  putStrLn $ "Code: " ++ pdfExample1
  runStringTyped pdfExample1 mZ

  putStrLn "\n--- PDF Example 2 (Rejected) ---"
  putStrLn $ "Code: " ++ pdfExample2
  runStringTyped pdfExample2 mZ

  putStrLn "\n--- Running Original Examples ---"
  putStrLn "\n--- Example 1 (mZ) ---"
  runStringTyped example1 mZ 

  putStrLn "\n--- Example 3 (m0) ---"
  runStringTyped example3 m0

  putStrLn "\n--- Example 4 (m0) ---"
  runStringTyped example4 m0
