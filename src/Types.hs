module Types where

import Algebra.Lattice
import Data.List (elemIndex)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Imp

transitiveClosure :: Int -> [(Int, Int)] -> [[Bool]]
transitiveClosure n relations = iterateClosure initial
  where
    matrix f = [[f i j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
    initial = matrix (\i j -> i == j || (i, j) `elem` relations)
    step m = matrix (\i j -> any (\k -> m !! i !! k && m !! k !! j) [0 .. n - 1])
    iterateClosure m =
      let next = step m
       in if next == m then m else iterateClosure next

computeJoinTable :: Int -> [[Bool]] -> [[Int]]
computeJoinTable n flows =
  [[findLUB i j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
  where
    findLUB i j =
      let upperBounds = [k | k <- [0 .. n - 1], flows !! i !! k && flows !! j !! k]
          isLUB l = all (\u -> flows !! l !! u) upperBounds
       in case filter isLUB upperBounds of
            (lub : _) -> lub
            [] -> error $ "Lattice error: No LUB for indices " ++ show i ++ " and " ++ show j

computeMeetTable :: Int -> [[Bool]] -> [[Int]]
computeMeetTable n flows =
  [[findGLB i j | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
  where
    findGLB i j =
      let lowerBounds = [k | k <- [0 .. n - 1], flows !! k !! i && flows !! k !! j]
          isGLB l = all (\lb -> flows !! lb !! l) lowerBounds
       in case filter isGLB lowerBounds of
            (glb : _) -> glb
            [] -> error $ "Lattice error: No GLB for indices " ++ show i ++ " and " ++ show j

mkSecurityLattice :: [String] -> [(String, String)] -> SecurityLattice
mkSecurityLattice names relations =
  let n = length names
      relIndices = [(maybe 0 id (elemIndex u names), maybe 0 id (elemIndex v names)) | (u, v) <- relations]
      flows = transitiveClosure n relIndices
      joins = computeJoinTable n flows
      meets = computeMeetTable n flows
      levels = [L name i joins meets flows names | (name, i) <- zip names [0 ..]]
   in SecurityLattice levels

liftLattice :: SecurityLattice -> SecurityLattice
liftLattice lat =
  let levels = latticeLevels lat
      names = lAllNames (head levels)
      n = length names
      flows = lFlowsTable (head levels)
      rels = [(names !! i, names !! j) | i <- [0 .. n - 1], j <- [0 .. n - 1], flows !! i !! j, i /= j]
      newNames = names ++ ["top_err"]
      newRels = rels ++ [(name, "top_err") | name <- names]
   in mkSecurityLattice newNames newRels
stdLatticeNames = ["bottom", "low", "high", "top"]

stdLatticeRelations = [("bottom", "low"), ("low", "high"), ("high", "top")]

stdLattice :: SecurityLattice
stdLattice = mkSecurityLattice stdLatticeNames stdLatticeRelations

bottomL = head (latticeLevels stdLattice)

lowL = (latticeLevels stdLattice) !! 1

highL = (latticeLevels stdLattice) !! 2

topL = (latticeLevels stdLattice) !! 3

updateEnv :: Environment -> VarName -> Level -> Environment
updateEnv env x l = \y -> if y == x then l else env y

joinEnv :: Environment -> Environment -> Environment
joinEnv env1 env2 = \x -> (env1 x) \/ (env2 x)

envFlowsTo :: [VarName] -> Environment -> Environment -> Bool
envFlowsTo vars env1 env2 = all (\x -> (env1 x) <= (env2 x)) vars

exprType :: Environment -> Expr -> Level
exprType _ (IntExpr _) = bottomL
exprType env (VarExpr x) = env x
exprType env (BinOpExpr _ e1 e2) =
  (exprType env e1) \/ (exprType env e2)

data TypeRes = WellTyped Environment Influences | TypeError String

-- Summary table for functions: (FunctionName, [ArgLevels], PCLevel) -> ReturnLevel
type FuncSummaries = Map.Map (String, [Level], Level) Level

cmdType' :: [Function] -> FuncSummaries -> [VarName] -> Environment -> (Level, Set.Set VarName) -> Influences -> Cmd -> TypeRes
cmdType' fns summaries vars env (pc, vars_pc_depends_on) infl cmd = case cmd of
  Skip -> WellTyped env infl
  Assign x e ->
    let l = exprType env e
        l' = pc \/ l
        -- Mirror the dynamic Assign: only clean x from other entries
        -- when the new x is independent of the old x. If the closure of
        -- the rhs's vars contains x (e.g. `x := x + 1`), the new x is
        -- in the same stream as the old x, so dependencies that other
        -- variables hold on x must be preserved.
        pre_closure = Imp.inflClosure (varsExpr e) infl
        new_depends_on_old = Set.member x pre_closure
        cleaned =
          if new_depends_on_old
            then infl
            else Map.map (Set.delete x) infl
        deps_closure = Imp.inflClosure (varsExpr e) cleaned
        new_infl = Map.insert x (Set.union deps_closure vars_pc_depends_on) cleaned
     in WellTyped (updateEnv env x l') new_infl
  Seq c1 c2 ->
    case cmdType' fns summaries vars env (pc, vars_pc_depends_on) infl c1 of
      WellTyped env' infl' -> cmdType' fns summaries vars env' (pc, vars_pc_depends_on) infl' c2
      err -> err
  If e c1 c2 ->
    let l = exprType env e
        pc' = pc \/ l
        vars_pc_depends_on' = Set.union vars_pc_depends_on (varsExpr e)
     in case cmdType' fns summaries vars env (pc', vars_pc_depends_on') infl c1 of
          WellTyped env1' infl1' -> case cmdType' fns summaries vars env (pc', vars_pc_depends_on') infl c2 of
            WellTyped env2' infl2' -> WellTyped (joinEnv env1' env2') (Map.unionWith Set.union infl1' infl2')
            err -> err
          err -> err
  While e c ->
    -- Fixed-point iteration over (env, infl): re-type the body under the
    -- joined post-state until nothing changes. The lattice has finite
    -- height and assignment is monotone in both env and infl (labels
    -- only join up, dependency sets only union in), so termination is
    -- bounded by O(height(lattice) × |vars|) iterations.
    let l = exprType env e
        pc' = pc \/ l
        vars_pc_depends_on' = Set.union vars_pc_depends_on (varsExpr e)

        loop env_in infl_in =
          case cmdType' fns summaries vars env_in (pc', vars_pc_depends_on') infl_in c of
            WellTyped env_out infl_out ->
              let env_next = joinEnv env_in env_out
                  infl_next = Map.unionWith Set.union infl_in infl_out
                  -- env_next ≥ env_in by construction; stability is
                  -- the reverse direction (env_next ≤ env_in).
                  env_stable = envFlowsTo vars env_next env_in
                  infl_stable = infl_next == infl_in
               in if env_stable && infl_stable
                    then WellTyped env_in infl_in
                    else loop env_next infl_next
            err -> err
     in loop env infl
  Input ch x ->
    if not (pc <= ch)
      then TypeError $ "Input failed: pc (" ++ show pc ++ ") does not flow to channel (" ++ show ch ++ ")"
      else
        let new_depends_on_old = Set.member x vars_pc_depends_on
            cleaned =
              if new_depends_on_old
                then infl
                else Map.map (Set.delete x) infl
            new_infl = Map.insert x vars_pc_depends_on cleaned
         in WellTyped (updateEnv env x (ch \/ pc)) new_infl
  Output ch e ->
    let l = exprType env e
     in if not ((pc \/ l) <= ch)
          then TypeError $ "Output failed: (pc join expr) (" ++ show (pc \/ l) ++ ") does not flow to channel (" ++ show ch ++ ")"
          else WellTyped env infl
  Erase l_cmd x ->
    let deps = getDependents x infl
        new_env =
          Set.foldl
            ( \acc_env v ->
                let l_v = acc_env v
                    l_target_v = l_cmd \/ l_v \/ pc
                 in updateEnv acc_env v l_target_v
            )
            env
            deps
        -- Same fix as the dynamic Erase: union vars_pc_depends_on into each erased
        -- variable's existing deps so the dependency chain survives the
        -- erase and a subsequent erase can still re-find the same set.
        new_infl =
          Set.foldl
            (\acc v -> Map.insertWith Set.union v vars_pc_depends_on acc)
            infl
            deps
     in WellTyped new_env new_infl
  Call x fName args ->
    case filter (\f -> funcName f == fName) fns of
      [] -> TypeError $ "Function " ++ fName ++ " not found"
      (f : _) ->
        let argLevels = map (exprType env) args
         in case Map.lookup (fName, argLevels, pc) summaries of
              Just retLevel ->
                -- Static can't peek into the callee's per-iteration influence
                -- map (the summary only carries retLevel), so we approximate
                -- the dynamic Return by taking the eager transitive closure of
                -- the argument variables within the caller's own infl.
                let arg_vars = Set.unions (map varsExpr args)
                    arg_closure = Imp.inflClosure arg_vars infl
                    new_x_deps = Set.union arg_closure vars_pc_depends_on
                    new_depends_on_old = Set.member x new_x_deps
                    cleaned =
                      if new_depends_on_old
                        then infl
                        else Map.map (Set.delete x) infl
                    new_infl = Map.insert x new_x_deps cleaned
                 in WellTyped (updateEnv env x (pc \/ retLevel)) new_infl
              Nothing ->
                TypeError $
                  "Function "
                    ++ fName
                    ++ " is not well-typed for argument levels "
                    ++ show argLevels
                    ++ " under PC "
                    ++ show pc
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
    top_err = last allLevels

    argCombos f = combinations (length (funcArgs f))
    combinations 0 = [[]]
    combinations n = [l : ls | l <- allLevels, ls <- combinations (n - 1)]

    typeCheckBody current f argL pcL =
      let initFEnv y = case lookup y (zip (funcArgs f) argL) of
            Just l -> l
            Nothing -> bot
          fVars = getVars (funcBody f)
          initFInfl = Map.fromList [(p, Set.empty) | p <- funcArgs f]
       in cmdType' fns current fVars initFEnv (pcL, Set.empty) initFInfl (funcBody f)

    initialAssumed =
      Map.fromList
        [ ((funcName f, argL, pcL), bot)
          | f <- fns,
            argL <- argCombos f,
            pcL <- allLevels
        ]

    iterateAssumed current =
      let next =
            Map.fromList
              [ ((funcName f, argL, pcL), retL)
                | f <- fns,
                  argL <- argCombos f,
                  pcL <- allLevels,
                  let retL = case typeCheckBody current f argL pcL of
                        WellTyped envAfter _ -> exprType envAfter (funcReturn f)
                        _ -> top_err
              ]
       in if next == current then current else iterateAssumed next

    assumed = iterateAssumed initialAssumed

    verified = Map.filter (/= top_err) assumed

cmdType lat fns vars env pc cmd =
  let summaries = computeSummaries (liftLattice lat) fns
      initialInfl = Map.empty
   in cmdType' fns summaries vars env (pc, Set.empty) initialInfl cmd

initEnv :: SecurityLattice -> Environment
initEnv lat = \_ -> head (latticeLevels lat)
