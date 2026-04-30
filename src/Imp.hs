module Imp where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub, elemIndex)
import Algebra.Lattice
import Debug.Trace (trace)

type VarName = String
type Value   = Integer

-- Generic Security Level
data Level = L {
    lName       :: String,
    lId         :: Int,
    lJoinTable  :: [[Int]],
    lMeetTable  :: [[Int]],
    lFlowsTable :: [[Bool]],
    lAllNames   :: [String] -- Keep names to allow consistent recreation
}

instance Eq Level where
    (L _ i1 _ _ _ _) == (L _ i2 _ _ _ _) = i1 == i2

instance Ord Level where
    l1 <= l2 = (lFlowsTable l1) !! (lId l1) !! (lId l2)
    compare l1 l2 
        | l1 == l2 = EQ
        | l1 <= l2 = LT
        | otherwise = GT

instance Show Level where
    show l = lName l

-- The Lattice instance now uses the pre-computed tables
instance Lattice Level where
    l1 \/ l2 = 
        let nextId = (lJoinTable l1) !! (lId l1) !! (lId l2)
        in l1 { lId = nextId, lName = (lAllNames l1) !! nextId }
    
    l1 /\ l2 = 
        let nextId = (lMeetTable l1) !! (lId l1) !! (lId l2)
        in l1 { lId = nextId, lName = (lAllNames l1) !! nextId }

-- Lattice representation
data SecurityLattice = SecurityLattice {
    latticeLevels :: [Level]
} deriving (Show, Eq)

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

data Program = Program SecurityLattice [Function] Cmd deriving (Eq, Show)

data Cmd = Skip | Assign VarName Expr | Seq Cmd Cmd
         | If Expr Cmd Cmd | While Expr Cmd
         | Input Level VarName | Output Level Expr
         | Erase Level VarName
         | Call VarName String [Expr]
         | Return
         | Stop
         | ResetPC Level -- Internal command to restore PC level
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
    varsCmd (Erase _ x) = Set.singleton x
    varsCmd (Call x _ args) = Set.insert x (Set.unions (map varsExpr args))
    varsCmd Return = Set.empty
    varsCmd Stop = Set.empty

-- Memory is a function from Variables to Values
type Memory = VarName -> Value

-- MultiMemory stores a memory view for each security level ID
type MultiMemory = Map.Map Int Memory

-- Dynamic labels for variables
type Labels = VarName -> Level

-- memory update
update :: Memory -> VarName -> Value -> Memory
update m x v = \y -> if y == x then v else m y

-- Configuration includes current command, multi-memory, labels, PC stack, input, output, and call stack
type Stack = [(VarName, MultiMemory, Labels, [Level], Expr)]
type Configuration = (Cmd, MultiMemory, Labels, [Level], [Value], [Value], Stack)

-- Helper to get the current memory view for a level
getMem :: MultiMemory -> Level -> Memory
getMem mm l = case Map.lookup (lId l) mm of
    Just m -> m
    Nothing -> \_ -> 0

-- Helper to update multi-memory for all levels >= targetLevel
updateMultiMemory :: SecurityLattice -> MultiMemory -> VarName -> Value -> Level -> MultiMemory
updateMultiMemory lat mm x v targetLevel =
    foldl (\acc l -> 
                if targetLevel <= l
                then Map.insert (lId l) (update (getMem acc l) x v) acc
                else acc) 
          mm (latticeLevels lat)

-- Helper to erase variable from multi-memory for all levels not >= targetLevel
eraseMultiMemory :: SecurityLattice -> MultiMemory -> VarName -> Level -> MultiMemory
eraseMultiMemory lat mm x targetLevel =
    foldl (\acc l -> 
                if not (targetLevel <= l)
                then Map.insert (lId l) (update (getMem acc l) x 0) acc
                else acc)
          mm (latticeLevels lat)

-- Expression level (join of all variables' labels)
getExprLevel :: Expr -> Labels -> Level -> Level
getExprLevel (IntExpr _) _ bottom = bottom
getExprLevel (VarExpr x) labs _ = labs x
getExprLevel (BinOpExpr _ e1 e2) labs bottom =
    (getExprLevel e1 labs bottom) \/ (getExprLevel e2 labs bottom)


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

step :: SecurityLattice -> [Function] -> Configuration -> Configuration

step _ _ (Skip, mm, labs, pcs, i, o, s) = (Stop, mm, labs, pcs, i, o, s)

step lat _ (Assign x e, mm, labs, pcs, i, o, s) =
    let pc = head pcs
        m_pc = getMem mm pc
        v = exprEval e m_pc
        bottom = head (latticeLevels lat)
        l_e = getExprLevel e labs bottom
        l_target = pc \/ l_e
        new_mm = updateMultiMemory lat mm x v l_target
        new_labs y = if y == x then l_target else labs y
    in (Stop, new_mm, new_labs, pcs, i, o, s)

step lat fns (Seq c1 c2, mm, labs, pcs, i, o, s) =
    let (c1', mm', labs', pcs', i', o', s') = step lat fns (c1, mm, labs, pcs, i, o, s)
    in case c1' of
          Stop -> (c2, mm', labs', pcs', i', o', s')
          _    -> (Seq c1' c2, mm', labs', pcs', i', o', s')

step lat _ (If e c1 c2, mm, labs, pcs, i, o, s) =
    let pc = head pcs
        m_pc = getMem mm pc
        bottom = head (latticeLevels lat)
        l_e = getExprLevel e labs bottom
        new_pc = pc \/ l_e
    in case exprEval e m_pc of
        0 -> (Seq c2 (ResetPC pc), mm, labs, new_pc : pcs, i, o, s)
        _ -> (Seq c1 (ResetPC pc), mm, labs, new_pc : pcs, i, o, s)

step _ _ (ResetPC old_pc, mm, labs, (_:pcs), i, o, s) = (Stop, mm, labs, old_pc : pcs, i, o, s)
step _ _ (ResetPC _, _, _, [], _, _, _) = error "PC stack underflow"

step lat _ (While e c, mm, labs, pcs, i, o, s) = 
    (If e (Seq c (While e c)) Skip, mm, labs, pcs, i, o, s)

step lat _ (Input ch x, mm, labs, pcs, i, [], s) = 
    let pc = head pcs
        l_target = ch \/ pc
        new_mm = updateMultiMemory lat mm x 0 l_target
        new_labs y = if y == x then l_target else labs y
    in (Stop, new_mm, new_labs, pcs, i, [], s)
step lat _ (Input ch x, mm, labs, pcs, (v:vs), o, s) =
    let pc = head pcs
        l_target = ch \/ pc
        new_mm = updateMultiMemory lat mm x v l_target
        new_labs y = if y == x then l_target else labs y
    in (Stop, new_mm, new_labs, pcs, vs, o, s)

step lat _ (Output ch e, mm, labs, pcs, i, o, s) =
    let m_ch = getMem mm ch
        v = exprEval e m_ch
    in (Stop, mm, labs, pcs, i, o ++ [v], s)

step lat _ (Erase l_cmd x, mm, labs, pcs, i, o, s) =
    let pc = head pcs
        l_var = labs x
        l_target = l_cmd \/ l_var \/ pc
        new_mm = eraseMultiMemory lat mm x l_target
        new_labs y = if y == x then l_target else labs y
    in (Stop, new_mm, new_labs, pcs, i, o, s)

step lat fns (Call x fName args, mm, labs, pcs, i, o, s) =
    case filter (\f -> funcName f == fName) fns of
        [] -> error $ "Function " ++ fName ++ " not found"
        (f:_) ->
            let pc = head pcs
                m_pc = getMem mm pc
                vals = map (\e -> exprEval e m_pc) args
                bottom = head (latticeLevels lat)
                l_args = map (\e -> getExprLevel e labs bottom) args
                
                -- Init local memory for all levels
                new_mm = foldl (\acc l -> 
                            let local_m = foldl (\m' (var, val) -> update m' var val) (\_ -> 0) (zip (funcArgs f) vals)
                            in Map.insert (lId l) local_m acc
                         ) Map.empty (latticeLevels lat)
                
                -- Init local labels
                new_labs y = case elemIndex y (funcArgs f) of
                                Just idx -> (l_args !! idx) \/ pc
                                Nothing -> bottom
                
            in (Seq (funcBody f) Return, new_mm, new_labs, [pc], i, o, (x, mm, labs, pcs, funcReturn f) : s)

step lat _ (Return, mm, labs, pcs, i, o, (x, caller_mm, caller_labs, caller_pcs, ret_expr) : s) =
    let pc = head pcs
        m_pc = getMem mm pc
        v = exprEval ret_expr m_pc
        bottom = head (latticeLevels lat)
        l_ret = getExprLevel ret_expr labs bottom
        l_target = l_ret \/ pc
        
        final_mm = updateMultiMemory lat caller_mm x v l_target
        final_labs y = if y == x then l_target else caller_labs y
    in (Stop, final_mm, final_labs, caller_pcs, i, o, s)

step _ _ (Stop, _, _, _, _, _, _) = error "impossible case"


-- INFRASTRUCTURE

data Result = Finished MultiMemory Labels [Value] | OutOfFuel

evalF :: Integer -> SecurityLattice -> [Function] -> Configuration -> Result
evalF 0 _ _ _ = OutOfFuel
evalF n lat fns config =
    let config'@(c', mm', labs', pcs', i', o', s') = step lat fns config
    in case c' of
        Stop | null s' -> Finished mm' labs' o'
        _              -> evalF (n-1) lat fns config'


-- print variables in vars on screen with their security level
printMultiMem :: MultiMemory -> Labels -> SecurityLattice -> [VarName] -> IO ()
printMultiMem mm labs lat vars = do
    putStrLn "--- Variable Labels ---"
    mapM_ (\x -> putStrLn $ x ++ ": " ++ show (labs x)) vars
    putStrLn "--- Memory Views ---"
    mapM_ (\l -> do
        putStrLn $ "Level " ++ show l ++ ":"
        let m = getMem mm l
        mapM_ (\x -> putStrLn $ "  " ++ x ++ ": " ++ show (m x)) vars
        ) (latticeLevels lat)

-- run program with fuel n and print the variable values and output
runF n lat fns vars (c, mm, labs, pcs, i, o, s) =
    case evalF n lat fns (c, mm, labs, pcs, i, o, s) of
        OutOfFuel  -> print "OutOfFuel"
        Finished mm' labs' o' -> do
            printMultiMem mm' labs' lat vars
            putStrLn $ "Output: " ++ show o'
