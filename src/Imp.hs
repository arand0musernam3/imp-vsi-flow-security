module Imp where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (elemIndex, intercalate)
import Algebra.Lattice
import Control.Monad (when)

-- ANSI color helpers (raw escape codes, no extra dependency needed)
color :: String -> String -> String
color code s = code ++ s ++ "\x1b[0m"

bold, dim, red, green, yellow, cyan :: String -> String
bold   = color "\x1b[1m"
dim    = color "\x1b[2m"
red    = color "\x1b[31m"
green  = color "\x1b[32m"
yellow = color "\x1b[33m"
cyan   = color "\x1b[36m"

boldRed, boldGreen, boldCyan, boldYellow :: String -> String
boldRed    = color "\x1b[1;31m"
boldGreen  = color "\x1b[1;32m"
boldCyan   = color "\x1b[1;36m"
boldYellow = color "\x1b[1;33m"

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

-- A stack frame holds the saved caller state plus, for the precise Approach B
-- of callee-to-caller dependency mapping:
--   * `calleeIntroNames` — every name introduced inside the callee (its params
--     plus all variables that appear in its body). Used at Return to drop
--     callee-only names from the dependency set.
--   * `argToVars` — for each parameter p, the set of caller-side variables
--     that appeared in the corresponding argument expression (joined with the
--     caller's pc_vars at the call site). Used at Return to substitute back
--     into the caller's namespace.
type StackFrame =
    ( VarName, MultiMemory, Labels, [PCContext], Expr, Influences
    , Set.Set VarName                    -- callee_intro_names
    , Map.Map VarName (Set.Set VarName)  -- arg_to_vars: param -> caller-side vars
    )
type Stack = [StackFrame]
type Configuration = (Cmd, MultiMemory, Labels, [PCContext], [Value], [(Level, Value)], Stack, Influences)

-- Forward transitive closure of `seed` through the influence map: starting
-- from a set of variables, repeatedly union in their direct dependencies until
-- nothing new appears. Used by Assign to keep `infl[x]` "eager" — every
-- transitive dependency is recorded directly, so dependency chains survive
-- subsequent reassignments that would otherwise drop intermediate vars.
-- (Named `inflClosure` to avoid clashing with `Types.transitiveClosure`,
-- which computes the reflexive-transitive closure of the lattice flow matrix.)
inflClosure :: Set.Set VarName -> Influences -> Set.Set VarName
inflClosure seed infl = go seed seed
  where
    go acc frontier
      | Set.null frontier = acc
      | otherwise =
          let next = Set.unions
                       [ Map.findWithDefault Set.empty v infl
                       | v <- Set.toList frontier ]
              new_frontier = Set.difference next acc
          in go (Set.union acc new_frontier) new_frontier

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

-- Helper to erase variable from multi-memory for all levels <= targetLevel 
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
        l_target = pc \/ l_e
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
                -- Decide whether the new x is independent of the old x. The
                -- new x carries information from the old x exactly when some
                -- variable on which it depends (transitively) is x itself —
                -- e.g. `x := x + 1`, or `x := y` after `y := x`. In that
                -- case the new x is in the same information stream as the
                -- old x, so anything in another variable's deps that was
                -- copied from old x is still relevant and must be kept.
                -- If instead the new x is truly fresh (`x := 5`, `x := y`
                -- where y doesn't transitively depend on x), variables that
                -- hold *historical* x-data are no longer connected to the
                -- variable named x; drop x from their dependency sets so a
                -- later `erase x` doesn't sweep them up.
                pre_closure = inflClosure (varsExpr e) infl
                new_depends_on_old = Set.member x pre_closure
                cleaned = if new_depends_on_old
                          then infl
                          else Map.map (Set.delete x) infl
                -- Eager transitive closure of e's variables: keeps dependency
                -- chains intact across reassignments (so e.g. inside a
                -- function body `t := a; t := t + 1` preserves t's
                -- dependency on a — and through a, on the caller's args —
                -- instead of collapsing infl[t] to {t}).
                deps_closure = inflClosure (varsExpr e) cleaned
                new_infl = Map.insert x (Set.union deps_closure pc_vars) cleaned
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

-- TODO (PC stack growth): desugaring While into If with a recursive While in
-- the then-branch means each iteration's If pushes a new PC context BEFORE the
-- previous iteration's ResetPC can fire — the outer ResetPC sits past the
-- recursive While in `Seq (Seq c (While e c)) ResetPC`. The stack therefore
-- grows by one frame per iteration during execution and only unwinds when the
-- loop finally terminates, costing O(iterations) memory in the configuration
-- (PC stack and the residual Cmd term). Correctness is unaffected — every push
-- is matched by a ResetPC at termination, and getPCLevel always returns the
-- correct (pc ⊔ l_e) since every pushed entry has the same label — but a long
-- loop will hold a linear-sized stack. A direct While rule that reuses one PC
-- frame per iteration would fix this; not worth the refactor today.
step mode lat _ (While e c, mm, labs, pcs, i, o, s, infl) =
    (If e (Seq c (While e c)) Skip, mm, labs, pcs, i, o, s, infl)

step mode lat _ (Input ch x, mm, labs, pcs, i, o, s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        l_target = ch \/ pc
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (pc <= ch)
       then error $ "Dynamic Monitor Exception: side-channel violation at input from channel "
                 ++ show ch ++ ". PC (" ++ show pc ++ ") does not flow to channel "
                 ++ show ch ++ "."
       -- NSU on the *current* label of x, not on the channel. `input` is a
       -- conditional update from the monitor's point of view: in one execution
       -- the branch is taken and labs x rises from labs_old → ch ⊔ pc; in the
       -- alternative execution it stays labs_old. If labs_old ≠ ch ⊔ pc, the
       -- label change is observable and leaks pc.
       --
       -- Concretely: `input(high, x); if x then input(high, y) else skip;
       --              if y then z := 0 else skip; output(low, z)`
       -- with x=0,y=0 leaves labs y = ⊥, so `if y` runs at pc=⊥ and the write
       -- to z is allowed — z observable at low encodes x.
       -- The (pc ⊑ ch) side-channel check above does NOT catch this case
       -- because pc=high ⊑ ch=high holds; only the NSU-on-labs-x check fires.
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
                    -- Input's new value comes from the external tape, so it
                    -- is independent of the old x in every case EXCEPT when
                    -- the input itself is conditional on x — i.e. we're
                    -- inside `if x then input(...) else …`. In that case x
                    -- is in pc_vars and the new x's existence carries info
                    -- about old x via implicit flow, so keep x in other
                    -- variables' dependency sets. Otherwise clean.
                    new_depends_on_old = Set.member x pc_vars
                    cleaned = if new_depends_on_old
                              then infl
                              else Map.map (Set.delete x) infl
                    new_infl = Map.insert x pc_vars cleaned
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
                -- Union pc_vars into each erased variable's existing
                -- dependency set, rather than replacing it. The erased
                -- variables are still in the same information stream as x —
                -- e.g. after `a := x; x := x+1; erase(_, x)`, both x and a
                -- get erased, but `a` is still derived from x and a later
                -- `erase(_, x)` must again pick `a` up as a dependent. The
                -- old `Map.insert v pc_vars` clobbered that link.
                new_infl = Set.foldl
                    (\acc v -> Map.insertWith Set.union v pc_vars acc)
                    infl deps
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

                -- Callee non-arg locals start at the caller's pc rather than
                -- bottom. This mirrors PLAS '09 [U-REF]: a freshly allocated
                -- cell takes the current pc as its label, so the first
                -- assignment in the body satisfies the NSU check `pc ⊑ label`.
                -- Without this, any function body containing an assignment is
                -- un-callable under a non-bottom pc.
                new_labs y = case elemIndex y (funcArgs f) of
                                Just idx -> arg_targets !! idx
                                Nothing  -> pc

                -- Callee_infl starts empty. The caller-side
                -- variables that contributed to each parameter are recorded
                -- separately in arg_to_vars and substituted back at Return.
                -- This avoids the name-shadowing pitfall of injecting caller
                -- variable names directly into the callee's infl map.
                new_infl = Map.empty

                callee_intro = Set.fromList (funcArgs f) `Set.union` varsCmd (funcBody f)
                arg_to_vars  = Map.fromList
                    [ (p, Set.union (varsExpr e) pc_vars)
                    | (p, e) <- zip (funcArgs f) args ]

                frame = (x, mm, labs, pcs, funcReturn f, infl, callee_intro, arg_to_vars)

            in (Seq (funcBody f) Return, new_mm, new_labs, [(pc, pc_vars)], i, o, frame : s, new_infl)

step mode lat _ (Return, mm, labs, pcs, i, o, (x, caller_mm, caller_labs, caller_pcs, ret_expr, caller_infl, callee_intro, arg_to_vars) : s, infl) =
    let pc = getPCLevel pcs
        pc_vars = getPCVars pcs
        caller_pc = getPCLevel caller_pcs
        caller_pc_vars = getPCVars caller_pcs
        bottom = head (latticeLevels lat)
        l_ret = getExprLevel ret_expr labs bottom
        l_target = l_ret \/ pc \/ caller_pc
        v = exprEval ret_expr (getMem mm l_target)
        monitorOn = mode == Dynamic || mode == Both
    in if monitorOn && not (caller_pc <= caller_labs x)
       then error $ "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at function return to "
                 ++ x ++ ". Caller PC (" ++ show caller_pc
                 ++ ") is not <= Current Label (" ++ show (caller_labs x) ++ ")."
       else let final_mm = updateMultiMemory lat caller_mm x v l_target
                final_labs y = if y == x then l_target else caller_labs y

                -- Approach B: take the forward transitive closure of the
                -- return-expression's variables (together with the callee's
                -- accumulated pc_vars) through the callee's influence map.
                -- Eager closure on Assign guarantees that this single closure
                -- captures every callee-local that the return value depends on.
                ret_seed = Set.union (varsExpr ret_expr) pc_vars
                callee_total = inflClosure ret_seed infl

                -- Resolve each name in the closure back into caller-side names:
                --   * if it's a parameter, substitute the caller variables in
                --     the corresponding argument expression (plus the caller's
                --     pc_vars at the call site, already merged in at Call);
                --   * else if it's a callee-introduced name, drop it (it has
                --     no meaning in the caller's namespace);
                --   * else keep it — it can only have arrived in the callee
                --     infl via caller-side pc_vars (e.g. an enclosing `if`).
                resolved = Set.unions
                    [ case Map.lookup n arg_to_vars of
                          Just caller_vars -> caller_vars
                          Nothing
                            | Set.member n callee_intro -> Set.empty
                            | otherwise                 -> Set.singleton n
                    | n <- Set.toList callee_total ]

                -- Same conditional cleanup as Assign: only drop x from
                -- existing caller-side dependency sets if the new x (the
                -- return value) is independent of the old x. The new x's
                -- deps are `resolved ∪ caller_pc_vars`, so the new x depends
                -- on the old x exactly when x appears in either — e.g.
                -- `x := call f(x)` (x reaches `resolved` through the args)
                -- or `if x then x := call f(0)` (x in caller_pc_vars via the
                -- enclosing branch).
                new_x_deps = Set.union resolved caller_pc_vars
                new_depends_on_old = Set.member x new_x_deps
                cleaned_caller_infl = if new_depends_on_old
                                      then caller_infl
                                      else Map.map (Set.delete x) caller_infl
                new_caller_infl = Map.insert x new_x_deps cleaned_caller_infl
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
    putStrLn $ boldCyan "  -- Security Report ----------------------------------------"
    putStrLn $ bold "  Outputs (by observer)"
    putStrLn $ "    " ++ padR (9 + obsW) "emitted" ++ " : " ++ show outputs
    mapM_ (\l ->
        let visible = [ v | (ch, v) <- outputs, ch <= l ]
        in putStrLn $ "    observer " ++ padR obsW (show l) ++ " : " ++ show visible
        ) levels
    putStrLn ""
    putStrLn $ bold "  Variable Visibility (final state)"
    let hdr = dim $ "    " ++ padR nameW "name" ++ "  " ++ padR labW "label"
              ++ concatMap (\l -> padL (valW + 1) (show l)) levels
    putStrLn hdr
    mapM_ (\x ->
        let row = "    " ++ padR nameW x ++ "  " ++ padR labW (show (labs x))
                  ++ concatMap (\l -> padL (valW + 1) (show (getMem mm l x))) levels
        in putStrLn row
        ) vars
    putStrLn ""
    putStrLn $ bold "  Influences"
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
            putStrLn $ bold "Output: " ++ show (map snd o')
            when showReport $ printSecurityReport lat vars mm' labs' o' infl'
