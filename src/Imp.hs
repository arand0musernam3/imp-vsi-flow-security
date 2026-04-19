module Imp where

import qualified Data.Set as Set

type VarName = String
type Value   = Integer

-- Generic Security Level
data Level = L String deriving (Eq, Ord)

instance Show Level where
    show (L s) = s

type Environment = VarName -> Level

data BinOp = Plus | Minus | Times
              deriving (Eq,Show)

data Expr  = IntExpr Value | VarExpr VarName | BinOpExpr BinOp Expr Expr
           deriving (Eq, Show)

data Cmd = Skip | Assign VarName Expr | Seq Cmd Cmd
         | If Expr Cmd Cmd | While Expr Cmd
         | Input Level VarName | Output Level Expr
         | Stop
           deriving (Eq, Show)

-- Get all variables used in a command
getVars :: Cmd -> [VarName]
getVars cmd = Set.toList (varsCmd cmd)
  where
    varsExpr (IntExpr _) = Set.empty
    varsExpr (VarExpr x) = Set.singleton x
    varsExpr (BinOpExpr _ e1 e2) = Set.union (varsExpr e1) (varsExpr e2)

    varsCmd Skip = Set.empty
    varsCmd (Assign x e) = Set.insert x (varsExpr e)
    varsCmd (Seq c1 c2) = Set.union (varsCmd c1) (varsCmd c2)
    varsCmd (If e c1 c2) = Set.unions [varsExpr e, varsCmd c1, varsCmd c2]
    varsCmd (While e c) = Set.union (varsExpr e) (varsCmd c)
    varsCmd (Input _ x) = Set.singleton x
    varsCmd (Output _ e) = varsExpr e
    varsCmd Stop = Set.empty

-- Memory is a function from Variables to Values
type Memory = VarName -> Value

-- memory update
update m x v = \y -> if y == x then v else m y

-- Configuration includes current command, memory, input stream, and output stream
type Configuration = (Cmd, Memory, [Value], [Value])


-- (Big-step) semantics of expressions
exprEval :: Expr -> Memory -> Value
exprEval (IntExpr n) _ = n
exprEval (VarExpr x) m = m x
exprEval (BinOpExpr binop e1 e2) m =
    let
        v1 = exprEval e1 m
        v2 = exprEval e2 m
    in (binOpSem binop) v1 v2
      where
            binOpSem Plus   = (+)
            binOpSem Minus  = (-)
            binOpSem Times  = (*)


-- SMALL-STEP SEMANTICS OF COMMANDS

step :: Configuration -> Configuration

step (Skip, m, i, o) = (Stop, m, i, o)

step (Assign x e, m, i, o) =
    let v = exprEval e m
    in (Stop, update m x v, i, o)

step (Seq c1 c2, m, i, o) =
    let (c1', m', i', o') = step (c1, m, i, o)
    in case c1' of
          Stop -> (c2, m', i', o')
          _    -> (Seq c1' c2, m', i', o')

step (If e c1 c2, m, i, o) =
    case exprEval e m of
        0 -> (c2, m, i, o)
        _ -> (c1, m, i, o)

step (While e c, m, i, o) = (If e (Seq c (While e c)) Skip, m, i, o)

step (Input _ x, m, [], o) = (Stop, update m x 0, [], o) -- Default to 0 if input empty
step (Input _ x, m, (v:vs), o) = (Stop, update m x v, vs, o)

step (Output _ e, m, i, o) =
    let v = exprEval e m
    in (Stop, m, i, o ++ [v])

step (Stop, _, _, _) = error "impossible case"


-- INFRASTRUCTURE

data Result = Finished Memory [Value] | OutOfFuel

evalF :: Integer -> Configuration -> Result
evalF 0 _ = OutOfFuel
evalF n config =
    let config'@(c', m', i', o') = step config
    in case c' of
        Stop -> Finished m' o'
        _    -> evalF (n-1) config'


-- print variables in vars on screen with their security level
printMem :: Memory -> Environment -> [VarName] -> IO ()
printMem m env = mapM_ ( \x ->  putStrLn (x ++ " (" ++ show (env x) ++ "): " ++  show (m x)) )

-- run program c with fuel n and print the variable values and output
runF n vars env (c, m, i, o) =
    case evalF n (c, m, i, o) of
        OutOfFuel  -> print "OutOfFuel"
        Finished m' o' -> do
            printMem m' env vars
            putStrLn $ "Output: " ++ show o'
