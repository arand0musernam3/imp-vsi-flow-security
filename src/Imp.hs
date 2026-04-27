module Imp where

import qualified Data.Set as Set
import qualified Data.Map as Map

type VarName = String
type Value   = Integer

-- Generic Security Level
newtype Level = L String deriving (Eq, Ord)

instance Show Level where
    show (L s) = s

type Environment = VarName -> Level

data BinOp = Plus | Minus | Times
              deriving (Eq,Show)

data Expr  = IntExpr Value | VarExpr VarName | BinOpExpr BinOp Expr Expr
           deriving (Eq, Show)

data Function = Function { 
    funcName   :: String, 
    funcArgs   :: [VarName], 
    funcBody   :: Cmd, 
    funcReturn :: Expr 
} deriving (Eq, Show)

data Program = Program [Function] Cmd deriving (Eq, Show)

data Cmd = Skip | Assign VarName Expr | Seq Cmd Cmd
         | If Expr Cmd Cmd | While Expr Cmd
         | Input Level VarName | Output Level Expr
         | Call VarName String [Expr]
         | Return
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
    varsCmd (While e c) = Set.union (varsCmd c) (varsCmd c)
    varsCmd (Input _ x) = Set.singleton x
    varsCmd (Output _ e) = varsExpr e
    varsCmd (Call x _ args) = Set.insert x (Set.unions (map varsExpr args))
    varsCmd Return = Set.empty
    varsCmd Stop = Set.empty

-- Memory is a function from Variables to Values
type Memory = VarName -> Value

-- memory update
update m x v = \y -> if y == x then v else m y

-- Configuration includes current command, memory, input stream, output stream, and call stack
type Stack = [(VarName, Memory, Expr)]
type Configuration = (Cmd, Memory, [Value], [Value], Stack)


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

step :: [Function] -> Configuration -> Configuration

step _ (Skip, m, i, o, s) = (Stop, m, i, o, s)

step _ (Assign x e, m, i, o, s) =
    let v = exprEval e m
    in (Stop, update m x v, i, o, s)

step fns (Seq c1 c2, m, i, o, s) =
    let (c1', m', i', o', s') = step fns (c1, m, i, o, s)
    in case c1' of
          Stop -> (c2, m', i', o', s')
          _    -> (Seq c1' c2, m', i', o', s')

step _ (If e c1 c2, m, i, o, s) =
    case exprEval e m of
        0 -> (c2, m, i, o, s)
        _ -> (c1, m, i, o, s)

step _ (While e c, m, i, o, s) = (If e (Seq c (While e c)) Skip, m, i, o, s)

step _ (Input _ x, m, [], o, s) = (Stop, update m x 0, [], o, s) -- Default to 0 if input empty
step _ (Input _ x, m, (v:vs), o, s) = (Stop, update m x v, vs, o, s)

step _ (Output _ e, m, i, o, s) =
    let v = exprEval e m
    in (Stop, m, i, o ++ [v], s)

step fns (Call x fName args, m, i, o, s) =
    case filter (\f -> funcName f == fName) fns of
        [] -> error $ "Function " ++ fName ++ " not found"
        (f:_) ->
            let vals = map (\e -> exprEval e m) args
                new_m = foldl (\m' (var, val) -> update m' var val) (\_ -> 0) (zip (funcArgs f) vals)
            in (Seq (funcBody f) Return, new_m, i, o, (x, m, funcReturn f) : s)

step _ (Return, m, i, o, (x, caller_m, ret_expr) : s) =
    let v = exprEval ret_expr m
    in (Stop, update caller_m x v, i, o, s)

step _ (Stop, _, _, _, _) = error "impossible case"


-- INFRASTRUCTURE

data Result = Finished Memory [Value] | OutOfFuel

evalF :: Integer -> [Function] -> Configuration -> Result
evalF 0 _ _ = OutOfFuel
evalF n fns config =
    let config'@(c', m', i', o', s') = step fns config
    in case c' of
        Stop | null s' -> Finished m' o'
        _              -> evalF (n-1) fns config'


-- print variables in vars on screen with their security level
printMem :: Memory -> Environment -> [VarName] -> IO ()
printMem m env = mapM_ ( \x ->  putStrLn (x ++ " (" ++ show (env x) ++ "): " ++  show (m x)) )

-- run program with fuel n and print the variable values and output
runF n fns vars env (c, m, i, o, s) =
    case evalF n fns (c, m, i, o, s) of
        OutOfFuel  -> print "OutOfFuel"
        Finished m' o' -> do
            printMem m' env vars
            putStrLn $ "Output: " ++ show o'
