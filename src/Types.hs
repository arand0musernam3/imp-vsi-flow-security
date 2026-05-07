module Types where

import Imp
import Data.List (elemIndex)
import qualified Data.Map as Map
import Algebra.Lattice

-- Transitive closure for the partial order
transitiveClosure :: Int -> [(Int, Int)] -> [[Bool]]
transitiveClosure n relations = 
    let initial = [ [ i == j || (i, j) `elem` relations | j <- [0..n-1] ] | i <- [0..n-1] ]
        step m = [ [ or [ m !! i !! k && m !! k !! j | k <- [0..n-1] ] | j <- [0..n-1] ] | i <- [0..n-1] ]
        iterateClosure m = let m' = step m in if m' == m then m else iterateClosure m'
    in iterateClosure initial

-- Compute Join table (Least Upper Bound)
computeJoinTable :: Int -> [[Bool]] -> [[Int]]
computeJoinTable n flows = 
    [ [ findLUB i j | j <- [0..n-1] ] | i <- [0..n-1] ]
  where
    findLUB i j = 
        let upperBounds = [ k | k <- [0..n-1], flows !! i !! k && flows !! j !! k ]
            isLUB l = all (\u -> flows !! l !! u) upperBounds
        in case filter isLUB upperBounds of
            (lub:_) -> lub
            [] -> error $ "Lattice error: No LUB for indices " ++ show i ++ " and " ++ show j

-- Compute Meet table (Greatest Lower Bound)
computeMeetTable :: Int -> [[Bool]] -> [[Int]]
computeMeetTable n flows = 
    [ [ findGLB i j | j <- [0..n-1] ] | i <- [0..n-1] ]
  where
    findGLB i j = 
        let lowerBounds = [ k | k <- [0..n-1], flows !! k !! i && flows !! k !! j ]
            isGLB l = all (\lb -> flows !! lb !! l) lowerBounds
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
        levels = [ L name i joins meets flows names | (name, i) <- zip names [0..] ]
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

data TypeRes = WellTyped Environment | TypeError String

-- Summary table for functions: (FunctionName, [ArgLevels], PCLevel) -> ReturnLevel
type FuncSummaries = Map.Map (String, [Level], Level) Level

-- Helper for typing with summaries
cmdType' :: [Function] -> FuncSummaries -> [VarName] -> Environment -> Level -> Cmd -> TypeRes
cmdType' fns summaries vars env pc cmd = case cmd of
    Skip -> WellTyped env
    Assign x e ->
        let l = exprType env e
            l' = pc \/ l
        in WellTyped (updateEnv env x l')
    Seq c1 c2 ->
        case cmdType' fns summaries vars env pc c1 of
            WellTyped env' -> cmdType' fns summaries vars env' pc c2
            err            -> err
    If e c1 c2 ->
        let l = exprType env e
            pc' = pc \/ l
        in case cmdType' fns summaries vars env pc' c1 of
            WellTyped env1' -> case cmdType' fns summaries vars env pc' c2 of
                WellTyped env2' -> WellTyped (joinEnv env1' env2')
                err -> err
            err -> err
    While e c ->
        let l = exprType env e
            pc' = pc \/ l
        in case cmdType' fns summaries vars env pc' c of
            WellTyped env' -> 
                if envFlowsTo vars env' env
                   then WellTyped env
                   else TypeError "While loop body changes environment unpredictably"
            err -> err
    Input ch x ->
        if not (pc <= ch)
        then TypeError $ "Input failed: pc (" ++ show pc ++ ") does not flow to channel (" ++ show ch ++ ")"
        else WellTyped (updateEnv env x (ch \/ pc))
    Output ch e ->
        let l = exprType env e
        in if not ((pc \/ l) <= ch)
        then TypeError $ "Output failed: (pc join expr) (" ++ show (pc \/ l) ++ ") does not flow to channel (" ++ show ch ++ ")"
        else WellTyped env
    Erase l_cmd x ->
        let l_var = env x
            l' = l_cmd \/ l_var \/ pc
        in WellTyped (updateEnv env x l')
    Call x fName args ->
        case filter (\f -> funcName f == fName) fns of
            [] -> TypeError $ "Function " ++ fName ++ " not found"
            (_:_) ->
                let argLevels = map (exprType env) args
                in case Map.lookup (fName, argLevels, pc) summaries of
                    Just retLevel -> WellTyped (updateEnv env x (pc \/ retLevel))
                    Nothing -> TypeError $ "Function " ++ fName
                              ++ " is not well-typed for argument levels "
                              ++ show argLevels ++ " under PC " ++ show pc
                              ++ "; refusing the call."
    Return -> WellTyped env
    Stop -> WellTyped env
    Halt -> WellTyped env
    -- ResetPC is inserted at runtime by step If; it never appears in
    -- parsed source, so the static type-checker never actually sees it.
    -- This case exists for exhaustiveness only.
    ResetPC -> WellTyped env

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
        in cmdType' fns current fVars initFEnv pcL (funcBody f)

    initialAssumed = Map.fromList [ ((funcName f, argL, pcL), bot)
                                  | f <- fns, argL <- argCombos f, pcL <- allLevels ]

    iterateAssumed current =
        let next = foldl (\acc f ->
                    foldl (\acc' argL ->
                        foldl (\acc'' pcL ->
                            case typeCheckBody current f argL pcL of
                                WellTyped envAfter ->
                                    let retL = exprType envAfter (funcReturn f)
                                    in Map.insert (funcName f, argL, pcL) retL acc''
                                _ -> acc''
                        ) acc' allLevels
                    ) acc (argCombos f)
                  ) current fns
        in if next == current then current else iterateAssumed next

    assumed = iterateAssumed initialAssumed

    verified = Map.fromList
        [ ((funcName f, argL, pcL), retL)
        | f <- fns, argL <- argCombos f, pcL <- allLevels
        , WellTyped envAfter <- [typeCheckBody assumed f argL pcL]
        , let retL = exprType envAfter (funcReturn f)
        ]

-- Wrap original cmdType to use summaries
cmdType lat fns vars env pc cmd =
    let summaries = computeSummaries lat fns
    in cmdType' fns summaries vars env pc cmd

-- EXAMPLES

-- levelFromName lives in Imp.hs (so the dynamic monitor can use it); it is
-- in scope here via `import Imp`.
initEnv :: SecurityLattice -> [VarName] -> Environment
initEnv lat vars =
  foldl (\env var -> updateEnv env var (levelFromName lat var)) (\_ -> head (latticeLevels lat)) vars
