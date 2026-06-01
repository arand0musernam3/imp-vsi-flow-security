module RegressionTests (runAllRegressionTests) where

import Imp
import TestUtils

runAllRegressionTests :: IO ()
runAllRegressionTests = do
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  putStrLn (bold "  Dynamic Monitor Regression Tests")
  putStrLn (bold (replicate 60 '='))

  results <-
    sequence
      [  
        -- LATTICE EXAMPLE
        runTest
          "Lattice example"
          Untyped
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          [("L1", 3)]
          ShouldPass
          True,
        runTest
          "Lattice example"
          DynamicNSU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          [("L1", 3)]
          ShouldFail
          True,
        runTest
          "Lattice example"
          DynamicPU
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          [("L1", 3)]
          ShouldFail
          True,
        staticTest
          "Lattice example"
          "lattice { Low < L1, Low < L2, L1 < High, L2 < High }; input(L1, x); output(L2, x)"
          ShouldFail,

        -- NSU / PU EXAMPLE
        runTest
          "NSU / PU example"
          Untyped
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [("high", 3)]
          ShouldPass
          True,
        runTest
          "NSU / PU example"
          DynamicNSU
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [("high", 3)]
          ShouldFail
          True,
        runTest
          "NSU / PU example"
          DynamicPU
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          [("high", 3)]
          ShouldFail
          True,
        staticTest
          "NSU / PU example"
          "input(high, secret); x := 0; if secret then x := 1 else skip; if x then output(high, 1) else output(high, 0)"
          ShouldPass,



        -- ERASURE EXAMPLE
        runTest
          "erasure example"
          Untyped
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 3)]
          ShouldPass
          True,
        runTest
          "erasure example"
          DynamicNSU
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 3)]
          ShouldFail
          True,
        runTest
          "erasure example"
          DynamicPU
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 3)]
          ShouldFail
          True,
        staticTest
          "erasure example"
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          ShouldFail,

        runTest
          "erasure example"
          Untyped
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 0)]
          ShouldPass
          True,
        runTest
          "erasure example"
          DynamicNSU
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 0)]
          ShouldPass
          True,
        runTest
          "erasure example"
          DynamicPU
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          [("bottom", 5), ("high", 0)]
          ShouldPass
          True,
        staticTest
          "erasure example"
          "input(high, s); input(bottom, x); y := x; if s then erase(high, x) else skip; if y then output(bottom,1) else output(bottom,0)"
          ShouldFail
      ]

  let passed = length (filter id results)
      total = length results
  putStrLn ""
  putStrLn (bold (replicate 60 '='))
  let summaryColor = if passed == total then boldGreen else boldRed
  putStrLn $ summaryColor ("  Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed")
  putStrLn (bold (replicate 60 '='))
