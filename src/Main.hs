module Main where

import Examples

main :: IO ()
main = do
  putStrLn "--- Running Example 1 (mZ) ---"
  runTyped example1 mZ 

  putStrLn "\n--- Running Example 3 (m0) ---"
  runTyped example3 m0

  putStrLn "\n--- Running Example 4 (m0) ---"
  runTyped example4 m0

  putStrLn "\n--- Running Example 4 (m1) ---"
  runTyped example4 m1 

  putStrLn "\n--- Running Exercise 2 (m0) ---"
  runTyped exercise2 m0 

  putStrLn "\n--- Running Exercise 2 (m1) ---"
  runTyped exercise2 m1
