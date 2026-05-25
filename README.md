# Dynamic vs Static Approach on Enforcing Non-Interference and Erasure on IMP

## Brief Project Description
In this project we take the toy-language IMP (found here: https://github.com/aslanix/imp-vsi-type-system) as a baseline. We will extend the language with inputs, outputs, erasure of variables and functions (first-order).

Furthermore, we compare the static approach with the dynamic approach to enforce non-interference and erasure in this extension of the language, with the respective formalizations. To accomplish the former, we extend the provided typing system. In regard to the latter, we extend the interpreter with a runtime monitoring tool.

## Installation & Running

1. Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. Build the project with `make build`. The resulting binary will be installed in the `./bin` directory in the current folder. 
3. Run the interpreter with `make run`.

## Binary arguments
If the program is run with no arguments, it executes the tests in `Main.hs`, which correspond to the examples shown in the report.

There are a lot of tests to check the correctness of the analysis and compiler, which can be run with the argument `--tests`.
