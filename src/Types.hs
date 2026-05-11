module Types where

import Imp
import Data.List (elemIndex)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Algebra.Lattice

-- Transitive closure for the partial order
transitiveClosure :: Int -> [(Int, Int)] -> [[Bool]]
transitiveClosure n relations = iterateClosure initial
  where
    -- Helper to build a matrix
    matrix f = [ [ f i j | j <- [0..n-1] ] | i <- [0..n-1] ]
    initial = matrix (\i j -> i == j || (i, j) `elem` relations) -- Inital matrix with givenflows and reflexivity
    step m = matrix  (\i j -> any (\k -> m !! i !! k && m !! k !! j) [0..n-1]) -- If there is a transitive path from i to j through k, then i flows to j

    -- recursivly apply step until no changes occur
    iterateClosure m = 
        let next = step m 
        in if next == m then m else iterateClosure next

-- Compute Join table (Least Upper Bound)
computeJoinTable :: Int -> [[Bool]] -> [[Int]]
computeJoinTable n flows = 
    [ [ findLUB i j | j <- [0..n-1] ] | i <- [0..n-1] ]
  where
    findLUB i j = 
        let upperBounds = [ k | k <- [0..n-1], flows !! i !! k && flows !! j !! k ] -- candidates that both i and j flow to
            isLUB l = all (\u -> flows !! l !! u) upperBounds -- check if l flows to all upper bounds, if so then it must be the least upper bound
        in case filter isLUB upperBounds of
            (lub:_) -> lub
            [] -> error $ "Lattice error: No LUB for indices " ++ show i ++ " and " ++ show j

-- Compute Meet table (Greatest Lower Bound)
computeMeetTable :: Int -> [[Bool]] -> [[Int]]
computeMeetTable n flows = 
    [ [ findGLB i j | j <- [0..n-1] ] | i <- [0..n-1] ]
  where
    findGLB i j = 
        let lowerBounds = [ k | k <- [0..n-1], flows !! k !! i && flows !! k !! j ]  -- candidates that flow to both i and j
            isGLB l = all (\lb -> flows !! lb !! l) lowerBounds -- check if all lower bounds flow to l, if so then it must be the greatest lower bound
        in case filter isGLB lowerBounds of
            (glb:_) -> glb
            [] -> error $ "Lattice error: No GLB for indices " ++ show i ++ " and " ++ show j

-- Factory to build the SecurityLattice
mkSecurityLattice :: [String] -> [(String, String)] -> SecurityLattice
mkSecurityLattice names relations =
    let n = length names
        relIndices = [ (maybe 0 id (elemIndex u names), maybe 0 id (elemIndex v names)) | (u, v) <- relations ]
        flows = transitiveClosure n relIndices
        joins = computeJoinTable n flows
        meets = computeMeetTable n flows
        levels = [ L name i joins meets flows names | (name, i) <- zip names [0..] ] -- L is a lattice level
    in SecurityLattice levels

-- Default levels
stdLatticeNames = ["bottom", "low", "high", "top"]
stdLatticeRelations = [("bottom", "low"), ("low", "high"), ("high", "top")]

stdLattice :: SecurityLattice
stdLattice = mkSecurityLattice stdLatticeNames stdLatticeRelations

bottomL = head (latticeLevels stdLattice)
lowL    = (latticeLevels stdLattice) !! 1
highL   = (latticeLevels stdLattice) !! 2
topL    = (latticeLevels stdLattice) !! 3

-- Flow-sensitive environment
updateEnv :: Environment -> VarName -> Level -> Environment
updateEnv env x l = \y -> if y == x then l else env y

joinEnv :: Environment -> Environment -> Environment
joinEnv env1 env2 = \x -> (env1 x) \/ (env2 x)

envFlowsTo :: [VarName] -> Environment -> Environment -> Bool
envFlowsTo vars env1 env2 = all (\x -> (env1 x) <= (env2 x)) vars

-- Expression Typing
exprType :: Environment -> Expr -> Level
exprType _   (IntExpr _ ) = bottomL -- Literals are bottom (public)
exprType env (VarExpr x ) = env x
exprType env (BinOpExpr _ e1 e2) =
    (exprType env e1) \/ (exprType env e2)

data TypeRes = WellTyped Environment Influences | TypeError String

-- Summary table for functions: (FunctionName, [ArgLevels], PCLevel) -> ReturnLevel
type FuncSummaries = Map.Map (String, [Level], Level) Level

-- Helper for typing with summaries
cmdType' :: [Function] -> FuncSummaries -> [VarName] -> Environment -> (Level, Set.Set VarName) -> Influences -> Cmd -> TypeRes
cmdType' fns summaries vars env (pc, pc_vars) infl cmd = case cmd of
    Skip -> WellTyped env infl
    Assign x e ->
        let l = exprType env e
            l' = pc \/ l
            new_infl = Map.insert x (Set.union (varsExpr e) pc_vars) infl
        in WellTyped (updateEnv env x l') new_infl
    Seq c1 c2 ->
        case cmdType' fns summaries vars env (pc, pc_vars) infl c1 of
            WellTyped env' infl' -> cmdType' fns summaries vars env' (pc, pc_vars) infl' c2
            err            -> err
    If e c1 c2 ->
        let l = exprType env e
            pc' = pc \/ l
            pc_vars' = Set.union pc_vars (varsExpr e)
        in case cmdType' fns summaries vars env (pc', pc_vars') infl c1 of
            WellTyped env1' infl1' -> case cmdType' fns summaries vars env (pc', pc_vars') infl c2 of
                WellTyped env2' infl2' -> WellTyped (joinEnv env1' env2') (Map.unionWith Set.union infl1' infl2')
                err -> err
            err -> err
    While e c ->
        let l = exprType env e
            pc' = pc \/ l
            pc_vars' = Set.union pc_vars (varsExpr e)
        in case cmdType' fns summaries vars env (pc', pc_vars') infl c of
            WellTyped env' infl' -> 
                if envFlowsTo vars env' env
                   then WellTyped env (Map.unionWith Set.union infl infl')
                   else TypeError "While loop body changes environment unpredictably"
            err -> err
    Input ch x ->
        if not (pc <= ch)
        then TypeError $ "Input failed: pc (" ++ show pc ++ ") does not flow to channel (" ++ show ch ++ ")"
        else let new_infl = Map.insert x pc_vars infl
             in WellTyped (updateEnv env x (ch \/ pc)) new_infl
    Output ch e ->
        let l = exprType env e
        in if not ((pc \/ l) <= ch)
        then TypeError $ "Output failed: (pc join expr) (" ++ show (pc \/ l) ++ ") does not flow to channel (" ++ show ch ++ ")"
        else WellTyped env infl
    Erase l_cmd x ->
        let deps = getDependents x infl
            new_env = Set.foldl (\acc_env v ->
                let l_v = acc_env v
                    l_target_v = l_cmd \/ l_v \/ pc
                in updateEnv acc_env v l_target_v
                ) env deps
            new_infl = Set.foldl (\acc v -> Map.insert v pc_vars acc) infl deps
        in WellTyped new_env new_infl
    Call x fName args ->
        case filter (\f -> funcName f == fName) fns of
            [] -> TypeError $ "Function " ++ fName ++ " not found"
            (f:_) ->
                let argLevels = map (exprType env) args
                in case Map.lookup (fName, argLevels, pc) summaries of
                    Just retLevel ->
                        let new_infl = Map.insert x (Set.union (Set.unions (map varsExpr args)) pc_vars) infl
                        in WellTyped (updateEnv env x (pc \/ retLevel)) new_infl
                    Nothing -> TypeError $ "Function " ++ fName
                              ++ " is not well-typed for argument levels "
                              ++ show argLevels ++ " under PC " ++ show pc
                              ++ "; refusing the call."
    Return -> WellTyped env infl
    Stop -> WellTyped env infl
    Halt -> WellTyped env infl
    -- ResetPC is inserted at runtime by step If; it never appears in
    -- parsed source, so the static type-checker never actually sees it.
    -- This case exists for exhaustiveness only.
    ResetPC -> WellTyped env infl

-- Compute function summaries in two phases:
--
--   Phase 1 (assumed) — fixed-point iteration starting from a permissive
--   "every function returns bottom" assumption. This map is only used
--   *internally* to seed recursion when type-checking function bodies; a
--   recursive call to f from within f's body needs *some* retLevel to
--   proceed, and bottom is the most permissive seed.
--
--   Phase 2 (verified) — re-type each function body under the converged
--   `assumed` map. Combos whose body type-checks at this point are exported
--   with the corresponding retLevel; combos whose body fails are *omitted*.
--   The caller-side cmdType' Call lookup turns a missing entry into a
--   TypeError, which is what closes the soundness gap: previously, a body
--   that fails for (argL, pcL) left the assumed-bottom entry in place and
--   the call silently succeeded with retLevel = bottom.
computeSummaries :: SecurityLattice -> [Function] -> FuncSummaries
computeSummaries lat fns = verified
  where
    allLevels = latticeLevels lat
    bot = head allLevels

    argCombos f = combinations (length (funcArgs f))
    combinations 0 = [[]]
    combinations n = [ l:ls | l <- allLevels, ls <- combinations (n-1) ]

    typeCheckBody current f argL pcL =
        let initFEnv y = case lookup y (zip (funcArgs f) argL) of
                            Just l -> l
                            Nothing -> bot
            fVars = getVars (funcBody f)
            initFInfl = Map.fromList [ (p, Set.empty) | p <- funcArgs f ]
        in cmdType' fns current fVars initFEnv (pcL, Set.empty) initFInfl (funcBody f)

    initialAssumed = Map.fromList [ ((funcName f, argL, pcL), bot)
                                  | f <- fns, argL <- argCombos f, pcL <- allLevels ]

    iterateAssumed current =
        let next = Map.fromList 
                [ ((funcName f, argL, pcL), retL)
                | f    <- fns
                , argL <- argCombos f
                , pcL  <- allLevels
                , let retL = case typeCheckBody current f argL pcL of
                               WellTyped envAfter _ -> exprType envAfter (funcReturn f)
                               _                    -> bot
                ]
        in if next == current then current else iterateAssumed next

    assumed = iterateAssumed initialAssumed

    verified = Map.fromList
        [ ((funcName f, argL, pcL), retL)
        | f <- fns, argL <- argCombos f, pcL <- allLevels
        , WellTyped envAfter _ <- [typeCheckBody assumed f argL pcL]
        , let retL = exprType envAfter (funcReturn f)
        ]

-- Wrap original cmdType to use summaries
cmdType lat fns vars env pc cmd =
    let summaries = computeSummaries lat fns
        initialInfl = Map.empty
    in cmdType' fns summaries vars env (pc, Set.empty) initialInfl cmd

-- EXAMPLES

-- levelFromName lives in Imp.hs (so the dynamic monitor can use it); it is
-- in scope here via `import Imp`.
initEnv :: SecurityLattice -> [VarName] -> Environment
initEnv lat vars =
  foldl (\env var -> updateEnv env var (levelFromName lat var)) (\_ -> head (latticeLevels lat)) vars
