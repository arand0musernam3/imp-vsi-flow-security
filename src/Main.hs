module Main where

import Control.Exception (SomeException, try)
import Examples
import Imp

data Expectation = ShouldPass | ShouldFail
    deriving (Eq, Show)

runTest :: String -> ExecMode -> String -> [Value] -> Expectation -> IO Bool
runTest name mode prog inputs expected = do
    putStrLn ""
    putStrLn (replicate 60 '-')
    putStrLn $ "TEST: " ++ name
    putStrLn (replicate 60 '-')
    putStrLn $ "  mode:    " ++ show mode
    putStrLn $ "  inputs:  " ++ show inputs
    putStrLn $ "  expect:  " ++ show expected
    putStrLn $ "  program: " ++ prog
    putStrLn ""
    result <- try (runStringModeWithInput mode prog inputs) :: IO (Either SomeException ())
    let (passed, msg) = case (result, expected) of
            (Left e,  ShouldFail) -> (True,  "expected error caught: " ++ shorten (show e))
            (Right _, ShouldPass) -> (True,  "completed without error")
            (Left e,  ShouldPass) -> (False, "unexpected error: " ++ shorten (show e))
            (Right _, ShouldFail) -> (False, "expected error but program completed")
    putStrLn ""
    putStrLn $ "  >>> " ++ (if passed then "PASS" else "FAIL") ++ " - " ++ msg
    return passed
  where
    shorten s = if length s > 250 then take 250 s ++ "..." else s

main :: IO ()
main = do
    putStrLn ""
    putStrLn (replicate 60 '=')
    putStrLn "  Dynamic Monitor Regression Tests"
    putStrLn (replicate 60 '=')

    results <- sequence
        [ runTest "NSU on assignment (branch taken, secret=1)"
                  Dynamic
                  "input(high, secret_s); x_p := 0; if secret_s then x_p := 1 else skip; output(low, x_p)"
                  [1]
                  ShouldFail

        , runTest "NSU on assignment (branch not taken, secret=0)"
                  Dynamic
                  "input(high, secret_s); x_p := 0; if secret_s then x_p := 1 else skip; output(low, x_p)"
                  [0]
                  ShouldPass

        , runTest "Explicit flow on output (secret to low channel)"
                  Dynamic
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldFail

        , runTest "Output well-typed (public to low channel)"
                  Dynamic
                  "x_p := 7; output(low, x_p)"
                  []
                  ShouldPass

        , runTest "Implicit flow on input (input under secret PC)"
                  Dynamic
                  "input(high, secret_s); if secret_s then input(low, x_p) else skip; output(low, x_p)"
                  [1, 99]
                  ShouldFail

        , runTest "NSU on function return (high pc, public target)"
                  Dynamic
                  "def f(a) { skip } return a; input(high, secret_s); x_p := 0; if secret_s then x_p := call f(0) else skip; output(low, x_p)"
                  [1]
                  ShouldFail

        , runTest "Empty input tape (runtime error)"
                  Dynamic
                  "input(low, x_p); output(low, x_p)"
                  []
                  ShouldFail

        , runTest "Untyped mode runs without monitor checks"
                  Untyped
                  "input(high, secret_s); output(low, secret_s)"
                  [42]
                  ShouldPass
        ]

    let passed = length (filter id results)
        total  = length results
    putStrLn ""
    putStrLn (replicate 60 '=')
    putStrLn $ "  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed"
    putStrLn (replicate 60 '=')
