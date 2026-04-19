module Main where

import Examples
import Parser (parseImp)

main :: IO ()
main = do
  putStrLn "\n--- Running Example 1 (mZ) ---"
  runStringTyped example1 mZ 

  putStrLn "\n--- Running Example 3 (m0) ---"
  runStringTyped example3 m0

  putStrLn "\n--- Running Example 4 (m0) ---"
  runStringTyped example4 m0

  putStrLn "\n--- Running Example 4 (m1) ---"
  runStringTyped example4 m1 

  putStrLn "\n--- Running Exercise 2 (m0) ---"
  runStringTyped exercise2 m0 

  putStrLn "\n--- Running Exercise 2 (m1) ---"
  runStringTyped exercise2 m1
