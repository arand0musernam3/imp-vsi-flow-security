{-# LANGUAGE RecordWildCards #-}

module Imp where

import Algebra.Lattice
import Data.List (elemIndex, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

type VarName = String

type Value = Integer

data Level = L
  { lName :: String,
    lId :: Int,
    lJoinTable :: [[Int]],
    lMeetTable :: [[Int]],
    lFlowsTable :: [[Bool]],
    lAllNames :: [String] -- Keep names to allow consistent recreation
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

instance Lattice Level where
  l1 \/ l2 =
    let nextId = (lJoinTable l1) !! (lId l1) !! (lId l2)
     in l1 {lId = nextId, lName = (lAllNames l1) !! nextId}

  l1 /\ l2 =
    let nextId = (lMeetTable l1) !! (lId l1) !! (lId l2)
     in l1 {lId = nextId, lName = (lAllNames l1) !! nextId}

data SecurityLattice = SecurityLattice
  { latticeLevels :: [Level]
  }
  deriving (Show, Eq)

type Environment = VarName -> Level

data BinOp = Plus | Minus | Times
  deriving (Eq, Show)

data Expr = IntExpr Value | VarExpr VarName | BinOpExpr BinOp Expr Expr
  deriving (Eq, Show)

data Function = Function
  { funcName :: String,
    funcArgs :: [VarName],
    funcBody :: Cmd,
    funcReturn :: Expr
  }
  deriving (Eq, Show)

data Program = Program SecurityLattice [Function] Cmd deriving (Eq, Show)

data Cmd
  = Skip
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
varsCmd ResetPC = Set.empty

getVars :: Cmd -> [VarName]
getVars cmd = Set.toList (varsCmd cmd)

type Memory = VarName -> Value

-- One memory view per security level, keyed by lId.
type MultiMemory = Map.Map Int Memory

type Labels = VarName -> Level

update :: Memory -> VarName -> Value -> Memory
update m x v = \y -> if y == x then v else m y

type PCContext = (Level, Set.Set VarName)

type Influences = Map.Map VarName (Set.Set VarName)

-- Partials: variables whose current value carries information about a branch
-- the monitor allowed under a non-flowing PC (Permissive Upgrade). Reading
-- such a variable as a branch condition aborts under DynamicPU. Always
-- empty under DynamicNSU / Untyped.
type Partials = Set.Set VarName

-- Saved caller state plus the two extras needed at Return to translate
-- callee-internal dependencies back into the caller's namespace:
--   * `calleeIntroNames` — names introduced inside the callee (params plus
--     body locals). Dropped from dependency sets at Return.
--   * `argToVars` — for each parameter p, the caller-side variables that
--     fed the i-th argument expression, joined with the caller's pc_vars
--     at the call site. Substituted in at Return.
type StackFrame =
  ( VarName,
    MultiMemory,
    Labels,
    [PCContext],
    Expr,
    Influences,
    Set.Set VarName, -- callee_intro_names
    Map.Map VarName (Set.Set VarName) -- arg_to_vars: param -> caller-side vars
  )

type Stack = [StackFrame]

-- The full machine state rewritten by the small-step semantics.
data Configuration = Configuration
  { cfgCmd :: Cmd, -- current command
    cfgMem :: MultiMemory, -- per-level memory views
    cfgLabs :: Labels, -- dynamic label environment Γ
    cfgPCs :: [PCContext], -- PC stack π
    cfgInput :: [(Level, Value)], -- input tape In (channel-tagged)
    cfgOutput :: [(Level, Value)], -- output tape Out
    cfgStack :: Stack, -- call stack S
    cfgInfl :: Influences, -- influence map I
    cfgPartials :: Partials -- partial-leak set P
  }

-- Fresh configuration: every variable at ⊥, every memory view zero, empty P.
initialConfig :: SecurityLattice -> Cmd -> [(Level, Value)] -> Configuration
initialConfig lat c is =
  let bottom = head (latticeLevels lat)
   in Configuration
        { cfgCmd = c,
          cfgMem = Map.fromList [(lId l, \_ -> 0) | l <- latticeLevels lat],
          cfgLabs = \_ -> bottom,
          cfgPCs = [(bottom, Set.empty)],
          cfgInput = is,
          cfgOutput = [],
          cfgStack = [],
          cfgInfl = Map.empty,
          cfgPartials = Set.empty
        }

-- Forward transitive closure of `seed` through the influence map: repeatedly
-- union in direct dependencies until nothing new appears. Named to avoid
-- clashing with `Types.transitiveClosure` (the lattice flow matrix).
inflClosure :: Set.Set VarName -> Influences -> Set.Set VarName
inflClosure seed infl = go seed seed
  where
    go acc frontier
      | Set.null frontier = acc
      | otherwise =
          let next =
                Set.unions
                  [ Map.findWithDefault Set.empty v infl
                    | v <- Set.toList frontier
                  ]
              new_frontier = Set.difference next acc
           in go (Set.union acc new_frontier) new_frontier

data ExecMode = Untyped | DynamicNSU | DynamicPU deriving (Eq, Show)

getMem :: MultiMemory -> Level -> Memory
getMem mm l = case Map.lookup (lId l) mm of
  Just m -> m
  Nothing -> \_ -> 0

-- Write v to x at every view ≥ targetLevel.
updateMultiMemory :: SecurityLattice -> MultiMemory -> VarName -> Value -> Level -> MultiMemory
updateMultiMemory lat mm x v targetLevel =
  foldl
    ( \acc l ->
        if targetLevel <= l
          then Map.insert (lId l) (update (getMem acc l) x v) acc
          else acc
    )
    mm
    (latticeLevels lat)

-- Channel-tagged input lookup: return the value at the first tape entry
-- whose channel is *exactly* the requested one (no flowing relation —
-- strict equality), along with the tape with that entry removed.
-- Inputs need not be in order: entries for other channels are skipped
-- but kept in place.
consumeFromChannel :: Level -> [(Level, Value)] -> Maybe (Value, [(Level, Value)])
consumeFromChannel ch = go []
  where
    go _ [] = Nothing
    go acc ((c, v) : rest)
      | c == ch = Just (v, reverse acc ++ rest)
      | otherwise = go ((c, v) : acc) rest

-- Zero x at every view ⋡ targetLevel (i.e. strictly below or incomparable).
eraseMultiMemory :: SecurityLattice -> MultiMemory -> VarName -> Level -> MultiMemory
eraseMultiMemory lat mm x targetLevel =
  foldl
    ( \acc l ->
        if not (targetLevel <= l)
          then Map.insert (lId l) (update (getMem acc l) x 0) acc
          else acc
    )
    mm
    (latticeLevels lat)

getExprLevel :: Expr -> Labels -> Level -> Level
getExprLevel (IntExpr _) _ bottom = bottom
getExprLevel (VarExpr x) labs _ = labs x
getExprLevel (BinOpExpr _ e1 e2) labs bottom =
  (getExprLevel e1 labs bottom) \/ (getExprLevel e2 labs bottom)

exprEval :: Expr -> Memory -> Value
exprEval (IntExpr n) _ = n
exprEval (VarExpr x) m = m x
exprEval (BinOpExpr binop e1 e2) m =
  let v1 = exprEval e1 m
      v2 = exprEval e2 m
   in (binOpSem binop) v1 v2
  where
    binOpSem Plus = (+)
    binOpSem Minus = (-)
    binOpSem Times = (*)

getPCLevel :: [PCContext] -> Level
getPCLevel ((l, _) : _) = l
getPCLevel [] = error "Empty PC stack"

getPCVars :: [PCContext] -> Set.Set VarName
getPCVars ((_, vs) : _) = vs
getPCVars [] = Set.empty

getDependents :: VarName -> Influences -> Set.Set VarName
getDependents y infl =
  let recurse todo acc
        | Set.null todo = acc
        | otherwise =
            let (v, rest) = Set.deleteFindMin todo
                new_deps = Set.fromList [x | (x, vs) <- Map.toList infl, v `Set.member` vs]
                next_todo = Set.union rest (Set.difference new_deps acc)
             in recurse next_todo (Set.insert v acc)
   in recurse (Set.singleton y) Set.empty

-- Small-step semantics. Each command constructor delegates to a per-rule
-- helper that reads the record fields it needs and returns the updated
-- configuration.
step :: ExecMode -> SecurityLattice -> [Function] -> Configuration -> Configuration
step mode lat fns cfg = case cfgCmd cfg of
  Skip -> cfg {cfgCmd = Stop}
  Stop -> error "impossible case"
  ResetPC -> stepResetPC cfg
  While e c -> cfg {cfgCmd = If e (Seq c (While e c)) Skip}
  If e c1 c2 -> stepIf mode lat cfg e c1 c2
  Seq c1 c2 -> stepSeq mode lat fns cfg c1 c2
  Assign x e -> stepAssign mode lat cfg x e
  Input ch x -> stepInput mode lat cfg ch x
  Output ch e -> stepOutput mode lat cfg ch e
  Erase l x -> stepErase mode lat cfg l x
  Call x fn args -> stepCall lat fns cfg x fn args
  Return -> stepReturn mode lat cfg



-- Pop the top PC frame on branch/loop exit.
stepResetPC :: Configuration -> Configuration
stepResetPC cfg = case cfgPCs cfg of
  (_ : pcs) -> cfg {cfgCmd = Stop, cfgPCs = pcs}
  [] -> error "PC stack underflow"

stepSeq :: ExecMode -> SecurityLattice -> [Function] -> Configuration -> Cmd -> Cmd -> Configuration
stepSeq mode lat fns cfg c1 c2 = 
    let cfg' = step mode lat fns (cfg {cfgCmd = c1})
     in case cfgCmd cfg' of
          Stop -> cfg' {cfgCmd = c2}
          c1' -> cfg' {cfgCmd = Seq c1' c2}

-- Under DynamicPU the deferred PU check fires here: if any variable read by
-- e is in P, abort.
stepIf :: ExecMode -> SecurityLattice -> Configuration -> Expr -> Cmd -> Cmd -> Configuration
stepIf mode lat cfg@Configuration {..} e c1 c2 =
  case puBranchViolation mode (varsExpr e) cfgPartials of
    Left msg -> error msg
    Right () ->
      let pc = getPCLevel cfgPCs
          pcVars = getPCVars cfgPCs
          bottom = head (latticeLevels lat)
          l_e = getExprLevel e cfgLabs bottom
          newPc = pc \/ l_e
          newPcVrs = Set.union pcVars (varsExpr e)
          taken = exprEval e (getMem cfgMem newPc) /= 0
          next = if taken then Seq c1 ResetPC else Seq c2 ResetPC
       in cfg {cfgCmd = next, cfgPCs = (newPc, newPcVrs) : cfgPCs}

stepAssign :: ExecMode -> SecurityLattice -> Configuration -> VarName -> Expr -> Configuration
stepAssign mode lat cfg@Configuration {..} x e =
  let pc = getPCLevel cfgPCs
      pcVars = getPCVars cfgPCs
      bottom = head (latticeLevels lat)
      l_e = getExprLevel e cfgLabs bottom
      l_target = pc \/ l_e
      v = exprEval e (getMem cfgMem l_target)
      curLab = cfgLabs x
      carriers = Set.union (varsExpr e) pcVars
   in case applyNsu mode pc curLab x carriers cfgPartials "assignment" of
        Left msg -> error msg
        Right newP ->
          let newMem = updateMultiMemory lat cfgMem x v l_target
              newLabs y = if y == x then l_target else cfgLabs y

              preClosure = inflClosure (varsExpr e) cfgInfl
              newDependsOnOld = Set.member x preClosure
              cleaned =
                if newDependsOnOld
                  then cfgInfl
                  else Map.map (Set.delete x) cfgInfl
              -- Eager closure keeps dependency chains intact across
              -- intermediate reassignments.
              depsClosure = inflClosure (varsExpr e) cleaned
              newInfl = Map.insert x (Set.union depsClosure pcVars) cleaned
           in cfg {cfgCmd = Stop, cfgMem = newMem, cfgLabs = newLabs, cfgInfl = newInfl, cfgPartials = newP}

-- not an NSU check; PU does NOT relax it.
stepInput :: ExecMode -> SecurityLattice -> Configuration -> Level -> VarName -> Configuration
stepInput mode lat cfg@Configuration {..} ch x =
  let pc = getPCLevel cfgPCs
      pcVars = getPCVars cfgPCs
      l_target = ch \/ pc
      monitorOn = mode == DynamicNSU || mode == DynamicPU
   in if monitorOn && not (pc <= ch)
        then
          error $
            "Dynamic Monitor Exception: side-channel violation at input from channel "
              ++ show ch
              ++ ". PC ("
              ++ show pc
              ++ ") does not flow to channel "
              ++ show ch
              ++ "."
        else case applyNsu mode pc (cfgLabs x) x pcVars cfgPartials "input" of
          Left msg -> error msg
          Right newP -> case consumeFromChannel ch cfgInput of
            Nothing ->
              error $
                "Runtime error: no input available on channel "
                  ++ show ch
                  ++ " for input to "
                  ++ x
                  ++ "."
            Just (v, vs) ->
              let newMem = updateMultiMemory lat cfgMem x v l_target
                  newLabs y = if y == x then l_target else cfgLabs y
                  newDependsOnOld = Set.member x pcVars
                  cleaned =
                    if newDependsOnOld
                      then cfgInfl
                      else Map.map (Set.delete x) cfgInfl
                  newInfl = Map.insert x pcVars cleaned
               in cfg {cfgCmd = Stop, cfgMem = newMem, cfgLabs = newLabs, cfgInput = vs, cfgInfl = newInfl, cfgPartials = newP}


stepOutput :: ExecMode -> SecurityLattice -> Configuration -> Level -> Expr -> Configuration
stepOutput mode lat cfg@Configuration {..} ch e =
  let v = exprEval e (getMem cfgMem ch)
      bottom = head (latticeLevels lat)
      l_e = getExprLevel e cfgLabs bottom
      pc = getPCLevel cfgPCs
      monitorOn = mode == DynamicNSU || mode == DynamicPU
   in if monitorOn && not ((l_e \/ pc) <= ch)
        then
          error $
            "Dynamic Monitor Exception: information flow violation at output to channel "
              ++ show ch
              ++ ". (ℓ_e="
              ++ show l_e
              ++ ") join (PC="
              ++ show pc
              ++ ") does not flow to channel "
              ++ show ch
              ++ "."
        else cfg {cfgCmd = Stop, cfgOutput = cfgOutput ++ [(ch, v)]}


stepErase :: ExecMode -> SecurityLattice -> Configuration -> Level -> VarName -> Configuration
stepErase mode lat cfg@Configuration {..} l_cmd x =
  let pc = getPCLevel cfgPCs
      l_var = cfgLabs x
      nsuFail = not (pc <= l_var)
   in if mode == DynamicNSU && nsuFail
        then
          error $
            "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at erase of "
              ++ x
              ++ ". PC ("
              ++ show pc
              ++ ") does not flow to gamma("
              ++ x
              ++ ") = "
              ++ show l_var
              ++ "; a conditional erase under a higher PC would be observable at lower levels."
        else
          let pcVars = getPCVars cfgPCs
              deps = getDependents x cfgInfl
              (newMem, newLabs) =
                Set.foldl
                  ( \(m, l) v ->
                      let l_target_v = l_cmd \/ l v \/ pc
                       in (eraseMultiMemory lat m v l_target_v, \z -> if z == v then l_target_v else l z)
                  )
                  (cfgMem, cfgLabs)
                  deps
              newInfl =
                Set.foldl
                  (\acc v -> Map.insertWith Set.union v pcVars acc)
                  cfgInfl
                  deps
              -- Under PU only: propagate P to every dependent when the
              -- erase itself was upgraded (nsuFail) or when x carried P.
              propagateP = nsuFail || Set.member x cfgPartials
              newP =
                if mode == DynamicPU && propagateP
                  then Set.union cfgPartials deps
                  else cfgPartials
           in cfg {cfgCmd = Stop, cfgMem = newMem, cfgLabs = newLabs, cfgInfl = newInfl, cfgPartials = newP}

-- Non-parameter locals start at the caller's pc rather than bot; without
-- this, any function body containing an assignment would be un-callable
-- under a non-bottom pc (the first assignment would fail NSU).
stepCall :: SecurityLattice -> [Function] -> Configuration -> VarName -> String -> [Expr] -> Configuration
stepCall lat fns cfg@Configuration {..} x fName args =
  case filter (\f -> funcName f == fName) fns of
    [] -> error $ "Function " ++ fName ++ " not found"
    (f : _) ->
      let pc = getPCLevel cfgPCs
          pcVars = getPCVars cfgPCs
          bottom = head (latticeLevels lat)
          l_args = map (\e -> getExprLevel e cfgLabs bottom) args
          argTgts = map (pc \/) l_args
          vals = zipWith (\e l_t -> exprEval e (getMem cfgMem l_t)) args argTgts
          emptyMem = Map.fromList [(lId l, \_ -> 0) | l <- latticeLevels lat]
          newMem =
            foldl
              (\acc (var, val, l_t) -> updateMultiMemory lat acc var val l_t)
              emptyMem
              (zip3 (funcArgs f) vals argTgts)
          newLabs y = case elemIndex y (funcArgs f) of
            Just idx -> argTgts !! idx
            Nothing -> pc
          calleeIntro = Set.fromList (funcArgs f) `Set.union` varsCmd (funcBody f)
          argToVars =
            Map.fromList
              [(p, Set.union (varsExpr e) pcVars) | (p, e) <- zip (funcArgs f) args]
          frame = (x, cfgMem, cfgLabs, cfgPCs, funcReturn f, cfgInfl, calleeIntro, argToVars)
       in cfg
            { cfgCmd = Seq (funcBody f) Return,
              cfgMem = newMem,
              cfgLabs = newLabs,
              cfgPCs = [(pc, pcVars)],
              cfgStack = frame : cfgStack,
              cfgInfl = Map.empty
            }

-- Under DynamicPU, callee-introduced P entries are dropped at the boundary;
-- they have no meaning in the caller's namespace. Caller-side P entries
-- survive.
stepReturn :: ExecMode -> SecurityLattice -> Configuration -> Configuration
stepReturn mode lat cfg@Configuration {..} = case cfgStack of
  [] -> error "Return with empty call stack"
  (x, callerMem, callerLabs, callerPCs, retExpr, callerInfl, calleeIntro, argToVars) : restStack ->
    let pc = getPCLevel cfgPCs
        pcVars = getPCVars cfgPCs
        callerPC = getPCLevel callerPCs
        callerPcVars = getPCVars callerPCs
        bottom = head (latticeLevels lat)
        l_ret = getExprLevel retExpr cfgLabs bottom
        l_target = l_ret \/ pc \/ callerPC
        v = exprEval retExpr (getMem cfgMem l_target)
        retSeed = Set.union (varsExpr retExpr) pcVars
        calleeTotal = inflClosure retSeed cfgInfl
        resolved =
          Set.unions
            [ case Map.lookup n argToVars of
                Just callerVars -> callerVars
                Nothing
                  | Set.member n calleeIntro -> Set.empty
                  | otherwise -> Set.singleton n
              | n <- Set.toList calleeTotal
            ]
        -- Drop callee-introduced P entries: they have no meaning in the
        -- caller's namespace. Caller-side P entries survive unchanged.
        cleanedP = Set.difference cfgPartials calleeIntro
     in case applyNsu mode callerPC (callerLabs x) x resolved cleanedP "function return" of
          Left msg -> error msg
          Right newP ->
            let finalMem = updateMultiMemory lat callerMem x v l_target
                finalLabs y = if y == x then l_target else callerLabs y
                newXDeps = Set.union resolved callerPcVars
                newDependsOnOld = Set.member x newXDeps
                cleanedCaller =
                  if newDependsOnOld
                    then callerInfl
                    else Map.map (Set.delete x) callerInfl
                newCallerInfl = Map.insert x newXDeps cleanedCaller
             in cfg
                  { cfgCmd = Stop,
                    cfgMem = finalMem,
                    cfgLabs = finalLabs,
                    cfgPCs = callerPCs,
                    cfgStack = restStack,
                    cfgInfl = newCallerInfl,
                    cfgPartials = newP
                  }

nsuMsg :: String -> VarName -> Level -> Level -> String
nsuMsg site x pc curLab =
  "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at "
    ++ site
    ++ " to "
    ++ x
    ++ ". PC ("
    ++ show pc
    ++ ") ⋢ Γ("
    ++ x
    ++ ") = "
    ++ show curLab
    ++ "."


applyNsu ::
  ExecMode ->
  Level ->
  Level ->
  VarName ->
  Set.Set VarName ->
  Partials ->
  String ->
  Either String Partials
applyNsu mode pc curLab x rhsCarriers p site =
  let nsuFail = not (pc <= curLab)
      pFromRhs = any (`Set.member` p) (Set.toList rhsCarriers)
      addToP = nsuFail || pFromRhs
      p' = if addToP then Set.insert x p else Set.delete x p
   in if mode == DynamicNSU && nsuFail
        then Left (nsuMsg site x pc curLab)
        else Right p'

-- Deferred PU check at branch conditions. Returns Left msg if PU must abort
-- because some variable read by the branch expression is P-marked; Right ()
-- otherwise. Modes other than DynamicPU never abort here.
puBranchViolation :: ExecMode -> Set.Set VarName -> Partials -> Either String ()
puBranchViolation DynamicPU eVars p
  | not (Set.null bad) = Left msg
  | otherwise = Right ()
  where
    bad = Set.intersection eVars p
    msg =
      "Dynamic Monitor Exception: Permissive-Upgrade violation at branch: "
        ++ "variables { "
        ++ intercalate ", " (Set.toList bad)
        ++ " } are partially-leaked (their values reflect a write made under "
        ++ "a non-flowing PC); branching on them would observe that decision."
puBranchViolation _ _ _ = Right ()

data Result = Finished MultiMemory Labels [(Level, Value)] Influences Partials | OutOfFuel

evalF :: Integer -> ExecMode -> SecurityLattice -> [Function] -> Configuration -> Result
evalF 0 _ _ _ _ = OutOfFuel
evalF n mode lat fns cfg =
  let cfg' = step mode lat fns cfg
      done = Finished (cfgMem cfg') (cfgLabs cfg') (cfgOutput cfg') (cfgInfl cfg') (cfgPartials cfg')
   in case cfgCmd cfg' of
        Stop | null (cfgStack cfg') -> done
        _ -> evalF (n - 1) mode lat fns cfg'
