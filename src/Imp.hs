module Imp where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (elemIndex, intercalate, isSuffixOf)
import Algebra.Lattice
import Control.Monad (when)

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

data Cmd = Skip
         | Assign VarName Expr
         | Seq Cmd Cmd
         | If Expr Cmd Cmd
         | While Expr Cmd
         | Input Level VarName
         | Output Level Expr
         | Erase Level VarName
         | Call VarName String [Expr]
         | Return
         | Stop -- Internal: "this command has finished"; absorbed by Seq.
         | Halt -- User-facing `stop` keyword: halts the entire program.
         | ResetPC -- Internal: pop the top of the PC stack on If/While exit.
           deriving (Eq, Show)

varsExpr :: Expr -> Set.Set VarName
varsExpr (IntExpr _) = Set.empty
varsExpr (VarExpr x) = Set.singleton x
varsExpr (BinOpExpr _ e1 e2) = Set.union (varsExpr e1) (varsExpr e2)

varsCmd :: Cmd -> Set.Set VarName
varsCmd Skip = Set.empty
varsCmd (Assign x e) = Set.insert x (varsExpr e)
varsCmd (Seq c1 c2) = Set.union (varsCmd c1) (varsCmd c2)
varsCmd (If e c1 c2) = Set.unions [varsExpr e, varsCmd c1, varsCmd c2]
varsCmd (While e c) = Set.union (varsExpr e) (varsCmd c)
varsCmd (Input _ x) = Set.singleton x
varsCmd (Output _ e) = varsExpr e
varsCmd (Erase _ x) = Set.singleton x
varsCmd (Call x _ args) = Set.insert x (Set.unions (map varsExpr args))
varsCmd Return = Set.empty
varsCmd Stop = Set.empty
varsCmd Halt = Set.empty
varsCmd ResetPC = Set.empty

-- Get all variables used in a command
getVars :: Cmd -> [VarName]
getVars cmd = Set.toList (varsCmd cmd)

-- Memory is a function from Variables to Values
type Memory = VarName -> Value

-- MultiMemory stores a memory view for each security level ID
type MultiMemory = Map.Map Int Memory

-- Dynamic labels for variables
type Labels = VarName -> Level

-- memory update
update :: Memory -> VarName -> Value -> Memory
update m x v = \y -> if y == x then v else m y

-- Configuration includes current command, multi-memory, labels, PC stack, input, output, call stack, and influences
type PCContext = (Level, Set.Set VarName)
type Influences = Map.Map VarName (Set.Set VarName)
type Stack = [(VarName, MultiMemory, Labels, [PCContext], Expr, Influences)]
type Configuration = (Cmd, MultiMemory, Labels, [PCContext], [Value], [(Level, Value)], Stack, Influences)

data ExecMode = Untyped | Static | Dynamic | Both deriving (Eq, Show)

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

-- Helper to erase variable from multi-memory for all levels <= targetLevel - TODO CHECK IF IT SHOULD BE <= OR NOT <=
eraseMultiMemory :: SecurityLattice -> MultiMemory -> VarName -> Level -> MultiMemory
eraseMultiMemory lat mm x targetLevel =
    foldl (\acc l ->
                if not (targetLevel <= l)
                then Map.insert (lId l) (update (getMem acc l) x 0) acc
                else acc)
          mm (latticeLevels lat)

-- Default label for a variable based on its name suffix:
--   *_p          → the lattice's "low"  level (public)
--   *_s          → the lattice's "high" level (secret)
--   anything else → the lattice's bottom level
-- The "low" / "high" levels are looked up by name in the given lattice, so
-- the convention works for custom lattices that include those names. If the
-- lattice has no level named "low" / "high", we fall back to bottom (safe
-- under-approximation; nothing flows below bottom).
levelFromName :: SecurityLattice -> VarName -> Level
levelFromName lat x =
    let levels = latticeLevels lat
        bot    = head levels
        byName n = case filter (\l -> lName l == n) levels of
                     (l:_) -> l
                     []    -> bot
    in if      "_p" `isSuffixOf` x then byName "low"
       else if "_s" `isSuffixOf` x then byName "high"
       else bot

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


getPCLevel :: [PCContext] -> Level
getPCLevel ((l, _):_) = l
getPCLevel [] = error "Empty PC stack"

getPCVars :: [PCContext] -> Set.Set VarName
getPCVars ((_, vs):_) = vs
getPCVars [] = Set.empty

getDependents :: VarName -> Influences -> Set.Set VarName
getDependents y infl =
    let recurse todo acc
          | Set.null todo = acc
          | otherwise =
              let (v, rest) = Set.deleteFindMin todo
                  new_deps = Set.fromList [ x | (x, vs) <- Map.toList infl, v `Set.member` vs ]
                  next_todo = Set.union rest (Set.difference new_deps acc)
              in recurse next_todo (Set.insert v acc)
    in recurse (Set.singleton y) Set.empty

-- SMALL-STEP SEMANTICS OF COMMANDS

step :: ExecMode -> SecurityLattice -> [Function] -> Configuration -> Configuration

step _ _ _ (Skip, mm, labs, pcs, i, o, s, infl) = (Stop, mm, labs, pcs, i, o, s, infl)
step mode lat _ (Assign x e, mm, labs, pcs, i, o, s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        bottom = head (latticeLevels lat)
        l_e = getExprLevel e labs bottom
        l_target = pc \/ l_e \/ labs x
        v = exprEval e (getMem mm l_target)
        cur_lab = labs x
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (pc <= cur_lab)
       then error $ "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at assignment to "
                 ++ x ++ ". Current label of " ++ x ++ " is " ++ show cur_lab
                 ++ " but PC is " ++ show pc ++ "; " ++ show pc
                 ++ " does not flow to " ++ show cur_lab ++ "."
       else let new_mm = updateMultiMemory lat mm x v l_target
                new_labs y = if y == x then l_target else labs y
                new_infl = Map.insert x (Set.union (varsExpr e) pc_vars) infl
            in (Stop, new_mm, new_labs, pcs, i, o, s, new_infl)

step mode lat fns (Seq c1 c2, mm, labs, pcs, i, o, s, infl) =
    case c1 of
        Halt -> (Halt, mm, labs, pcs, i, o, s, infl)
        _ ->
            let (c1', mm', labs', pcs', i', o', s', infl') = step mode lat fns (c1, mm, labs, pcs, i, o, s, infl)
            in case c1' of
                  Stop -> (c2,            mm', labs', pcs', i', o', s', infl')
                  Halt -> (Halt,          mm', labs', pcs', i', o', s', infl')
                  _    -> (Seq c1' c2,    mm', labs', pcs', i', o', s', infl')

step _ lat _ (If e c1 c2, mm, labs, pcs, i, o, s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        bottom = head (latticeLevels lat)
        l_e = getExprLevel e labs bottom
        new_pc = pc \/ l_e
        new_pc_vars = Set.union pc_vars (varsExpr e)
        m_new_pc = getMem mm new_pc
    in case exprEval e m_new_pc of
        0 -> (Seq c2 ResetPC, mm, labs, (new_pc, new_pc_vars) : pcs, i, o, s, infl)
        _ -> (Seq c1 ResetPC, mm, labs, (new_pc, new_pc_vars) : pcs, i, o, s, infl)

-- ResetPC pops the PC stack
step _ _ _ (ResetPC, mm, labs, (_:pcs), i, o, s, infl) = (Stop, mm, labs, pcs, i, o, s, infl)
step _ _ _ (ResetPC, _, _, [], _, _, _, _) = error "PC stack underflow"

step mode lat _ (While e c, mm, labs, pcs, i, o, s, infl) =
    (If e (Seq c (While e c)) Skip, mm, labs, pcs, i, o, s, infl)

step mode lat _ (Input ch x, mm, labs, pcs, i, o, s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        l_target = ch \/ pc \/ labs x
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (pc <= ch)
       then error $ "Dynamic Monitor Exception: side-channel violation at input from channel "
                 ++ show ch ++ ". PC (" ++ show pc ++ ") does not flow to channel "
                 ++ show ch ++ "."
       else if monitorOn && not (pc <= labs x)
       then error $ "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at input to "
                 ++ x ++ ". PC (" ++ show pc ++ ") is not <= Current Label ("
                 ++ show (labs x) ++ ")."
       else case i of
            [] -> error $ "Runtime error: input tape exhausted at input to " ++ x
                       ++ " on channel " ++ show ch ++ "." -- TODO REVISE
            (v:vs) ->
                let new_mm = updateMultiMemory lat mm x v l_target
                    new_labs y = if y == x then l_target else labs y
                    new_infl = Map.insert x pc_vars infl -- Input depends on PC vars (which channel we read from might be conditional)
                in (Stop, new_mm, new_labs, pcs, vs, o, s, new_infl)

step mode lat _ (Output ch e, mm, labs, pcs, i, o, s, infl) =
    -- Note on the memory view: Assign / Return / If read at m_(pc ⊔ l_e),
    -- but Output reads at m_ch instead. That's correct because the flow
    -- check below requires (l_e ⊔ pc) ⊑ ch, i.e. ch dominates the target
    -- view, and a higher view contains at least as much information as a
    -- lower one. If the check fails we error out before consuming v, so
    -- the read is never observable in the failing case. TODO REWRITE
    let m_ch = getMem mm ch
        v = exprEval e m_ch
        bottom = head (latticeLevels lat)
        l_e = getExprLevel e labs bottom
        pc = getPCLevel pcs
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not ((l_e \/ pc) <= ch)
       then error $ "Dynamic Monitor Exception: information flow violation at output to channel "
                 ++ show ch ++ ". Expression label (" ++ show l_e ++ ") joined with PC ("
                 ++ show pc ++ ") is not <= channel label " ++ show ch ++ "."
       else (Stop, mm, labs, pcs, i, o ++ [(ch, v)], s, infl)

step mode lat _ (Erase l_cmd x, mm, labs, pcs, i, o, s, infl) =
    let pc = getPCLevel pcs
        l_var = labs x
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (pc <= l_var)
       then error $ "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at erase of "
                 ++ x ++ ". PC (" ++ show pc ++ ") is not <= Current Label ("
                 ++ show l_var ++ "); a conditional erase under a higher PC would "
                 ++ "be observable at lower levels."
       else let pc_vars = getPCVars pcs
                deps = getDependents x infl
                (new_mm, new_labs) = Set.foldl (\(m, l) v ->
                    let l_target_v = l_cmd \/ l v \/ pc
                    in (eraseMultiMemory lat m v l_target_v, \z -> if z == v then l_target_v else l z)
                    ) (mm, labs) deps
                new_infl = Set.foldl (\acc v -> Map.insert v pc_vars acc) infl deps
            in (Stop, new_mm, new_labs, pcs, i, o, s, new_infl)
step mode lat fns (Call x fName args, mm, labs, pcs, i, o, s, infl) =
    case filter (\f -> funcName f == fName) fns of
        [] -> error $ "Function " ++ fName ++ " not found"
        (f:_) ->
            let pc = getPCLevel pcs
                pc_vars = getPCVars pcs
                bottom = head (latticeLevels lat)
                l_args = map (\e -> getExprLevel e labs bottom) args -- Arguments levels
                arg_targets = map (pc \/) l_args -- Raises each arg level to at least pc.

                vals = zipWith (\e l_t -> exprEval e (getMem mm l_t)) args arg_targets --Evaluate each argument


                empty_mm = Map.fromList [ (lId l, \_ -> 0) | l <- latticeLevels lat ] -- Empty_memory for the function
                new_mm = foldl (\acc (var, val, l_t) ->
                                  updateMultiMemory lat acc var val l_t)
                               empty_mm
                               (zip3 (funcArgs f) vals arg_targets) -- Update the memory with the function arguments value (vals)

                new_labs y = case elemIndex y (funcArgs f) of
                                Just idx -> arg_targets !! idx
                                Nothing  -> levelFromName lat y

                -- Callee influences: parameters depend on the variables in the arguments and caller PC
                new_infl = Map.fromList [ (p, Set.union (varsExpr e) pc_vars)
                                        | (p, e) <- zip (funcArgs f) args ]

            in (Seq (funcBody f) Return, new_mm, new_labs, [(pc, pc_vars)], i, o, (x, mm, labs, pcs, funcReturn f, infl) : s, new_infl)

step mode lat _ (Return, mm, labs, pcs, i, o, (x, caller_mm, caller_labs, caller_pcs, ret_expr, caller_infl) : s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        caller_pc = getPCLevel caller_pcs
        caller_pc_vars = getPCVars caller_pcs
        bottom = head (latticeLevels lat)
        l_ret = getExprLevel ret_expr labs bottom
        l_target = l_ret \/ pc \/ caller_pc \/ caller_labs x
        v = exprEval ret_expr (getMem mm l_target)
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (caller_pc <= caller_labs x)
       then error $ "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at function return to "
                 ++ x ++ ". Caller PC (" ++ show caller_pc
                 ++ ") is not <= Current Label (" ++ show (caller_labs x) ++ ")."
       else let final_mm = updateMultiMemory lat caller_mm x v l_target
                final_labs y = if y == x then l_target else caller_labs y
                -- Return value depends on variables that influenced ret_expr and callee PC.
                -- These influences in 'infl' are callee-local. We map them back to the caller.
                callee_deps = Set.union (varsExpr ret_expr) pc_vars
                -- For each callee_dep, find what it depended on in the caller (via initial new_infl in Call)
                -- Actually, callee 'infl' already tracks dependencies back to caller variables!
                -- Because 'new_infl' in 'Call' initialized parameters with caller vars.
                total_caller_deps = Set.unions [ Map.findWithDefault Set.empty d infl | d <- Set.toList callee_deps ]
                new_caller_infl = Map.insert x (Set.union total_caller_deps caller_pc_vars) caller_infl
            in (Stop, final_mm, final_labs, caller_pcs, i, o, s, new_caller_infl)

step _ _ _ (Halt, mm, labs, pcs, i, o, s, infl) = (Halt, mm, labs, pcs, i, o, s, infl)

step _ _ _ (Stop, _, _, _, _, _, _, _) = error "impossible case"

-- INFRASTRUCTURE

data Result = Finished MultiMemory Labels [(Level, Value)] Influences | OutOfFuel

evalF :: Integer -> ExecMode -> SecurityLattice -> [Function] -> Configuration -> Result
evalF 0 _ _ _ _ = OutOfFuel
evalF n mode lat fns config =
    let config'@(c', mm', labs', pcs', i', o', s', infl') = step mode lat fns config
    in case c' of
        Halt           -> Finished mm' labs' o' infl' -- `stop` halts regardless of stack depth
        Stop | null s' -> Finished mm' labs' o' infl'
        _              -> evalF (n-1) mode lat fns config'


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

printSecurityReport :: SecurityLattice -> [VarName] -> MultiMemory -> Labels -> [(Level, Value)] -> Influences -> IO ()
printSecurityReport lat vars mm labs outputs infl = do
    let levels = latticeLevels lat
        obsW   = maximum (1 : map (length . show) levels)
        nameW  = maximum (4 : map length vars)
        labW   = maximum (5 : map (length . show . labs) vars)
        valW   = maximum (6 : map (length . show) levels
                            ++ [ length (show (getMem mm l x)) | x <- vars, l <- levels ])
    putStrLn "  -- Security Report ----------------------------------------"
    putStrLn "  Outputs (by observer)"
    putStrLn $ "    " ++ padR (9 + obsW) "emitted" ++ " : " ++ show outputs
    mapM_ (\l ->
        let visible = [ v | (ch, v) <- outputs, ch <= l ]
        in putStrLn $ "    observer " ++ padR obsW (show l) ++ " : " ++ show visible
        ) levels
    putStrLn ""
    putStrLn "  Variable Visibility (final state)"
    let hdr = "    " ++ padR nameW "name" ++ "  " ++ padR labW "label"
              ++ concatMap (\l -> padL (valW + 1) (show l)) levels
    putStrLn hdr
    mapM_ (\x ->
        let row = "    " ++ padR nameW x ++ "  " ++ padR labW (show (labs x))
                  ++ concatMap (\l -> padL (valW + 1) (show (getMem mm l x))) levels
        in putStrLn row
        ) vars
    putStrLn ""
    putStrLn "  Influences"
    mapM_ (\x ->
        let deps    = Map.findWithDefault Set.empty x infl
            depsStr = if Set.null deps
                      then "\8709"
                      else "{ " ++ intercalate ", " (Set.toList deps) ++ " }"
        in putStrLn $ "    " ++ x ++ "  \8592  " ++ depsStr
        ) vars
    putStrLn ""
  where
    padR n s = s ++ replicate (max 0 (n - length s)) ' '
    padL n s = replicate (max 0 (n - length s)) ' ' ++ s

-- run program with fuel n and print the variable values and output
runF n showReport mode lat fns vars (c, mm, labs, pcs, i, o, s, infl) =
    case evalF n mode lat fns (c, mm, labs, pcs, i, o, s, infl) of
        OutOfFuel  -> print "OutOfFuel"
        Finished mm' labs' o' infl' -> do
            putStrLn $ "Output: " ++ show (map snd o')
            when showReport $ printSecurityReport lat vars mm' labs' o' infl'
