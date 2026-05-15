{-# LANGUAGE RecordWildCards #-}

module Imp where

import Algebra.Lattice
import Data.List (elemIndex, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

type VarName = String

type Value = Integer

-- Generic Security Level
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

-- The Lattice instance now uses the pre-computed tables
instance Lattice Level where
  l1 \/ l2 =
    let nextId = (lJoinTable l1) !! (lId l1) !! (lId l2)
     in l1 {lId = nextId, lName = (lAllNames l1) !! nextId}

  l1 /\ l2 =
    let nextId = (lMeetTable l1) !! (lId l1) !! (lId l2)
     in l1 {lId = nextId, lName = (lAllNames l1) !! nextId}

-- Lattice representation
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
  | Halt -- Internal: halts the entire program (not user-writable).
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

type PCContext = (Level, Set.Set VarName)

type Influences = Map.Map VarName (Set.Set VarName)

-- Partials: variables whose current value carries information about a branch
-- the monitor allowed under a non-flowing PC (Permissive Upgrade). Reading
-- such a variable as a branch condition aborts under DynamicPU. Always
-- empty under DynamicNSU / Untyped.
type Partials = Set.Set VarName

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

-- A configuration bundles the entire machine state that the small-step
-- semantics rewrites. See proud-enchanting-mountain.md Part I.8 for the
-- mathematical specification.
data Configuration = Configuration
  { cfgCmd      :: Cmd                -- current command
  , cfgMem      :: MultiMemory        -- per-level memory views
  , cfgLabs     :: Labels             -- dynamic label environment Γ
  , cfgPCs      :: [PCContext]        -- PC stack π
  , cfgInput    :: [Value]            -- input tape In
  , cfgOutput   :: [(Level, Value)]   -- output tape Out
  , cfgStack    :: Stack              -- call stack S
  , cfgInfl     :: Influences         -- influence map I
  , cfgPartials :: Partials           -- partial-leak set P (used only by DynamicPU)
  }

-- Build the initial configuration κ₀ for command c on input tape `is`.
-- Variables are at ⊥, every memory view is the all-zero map, the PC stack
-- carries a single ⊥-context, the partial-leak set is empty.
initialConfig :: SecurityLattice -> Cmd -> [Value] -> Configuration
initialConfig lat c is =
  let bottom = head (latticeLevels lat)
   in Configuration
        { cfgCmd      = c
        , cfgMem      = Map.fromList [(lId l, \_ -> 0) | l <- latticeLevels lat]
        , cfgLabs     = \_ -> bottom
        , cfgPCs      = [(bottom, Set.empty)]
        , cfgInput    = is
        , cfgOutput   = []
        , cfgStack    = []
        , cfgInfl     = Map.empty
        , cfgPartials = Set.empty
        }

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
          let next =
                Set.unions
                  [ Map.findWithDefault Set.empty v infl
                    | v <- Set.toList frontier
                  ]
              new_frontier = Set.difference next acc
           in go (Set.union acc new_frontier) new_frontier

data ExecMode = Untyped | DynamicNSU | DynamicPU deriving (Eq, Show)

-- Future variants: DynamicFaceted, ...

-- Helper to get the current memory view for a level
getMem :: MultiMemory -> Level -> Memory
getMem mm l = case Map.lookup (lId l) mm of
  Just m -> m
  Nothing -> \_ -> 0

-- Helper to update multi-memory for all levels >= targetLevel
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

-- Helper to erase variable from multi-memory for all levels <= targetLevel
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

-- SMALL-STEP SEMANTICS OF COMMANDS

-- Top-level dispatch. Each command constructor delegates to a per-rule
-- helper. Helpers read the record fields they need (via RecordWildCards)
-- and return the updated configuration with record-update syntax. The
-- structure mirrors proud-enchanting-mountain.md Part VII: every M-Foo
-- rule corresponds 1:1 to a stepFoo function below.
step :: ExecMode -> SecurityLattice -> [Function] -> Configuration -> Configuration
step mode lat fns cfg = case cfgCmd cfg of
  Skip            -> cfg {cfgCmd = Stop}
  Halt            -> cfg
  Stop            -> error "impossible case"
  ResetPC         -> stepResetPC cfg
  While e c       -> cfg {cfgCmd = If e (Seq c (While e c)) Skip}
  If e c1 c2      -> stepIf mode lat cfg e c1 c2
  Seq c1 c2       -> stepSeq mode lat fns cfg c1 c2
  Assign x e      -> stepAssign mode lat cfg x e
  Input ch x      -> stepInput mode lat cfg ch x
  Output ch e     -> stepOutput mode lat cfg ch e
  Erase l x       -> stepErase mode lat cfg l x
  Call x fn args  -> stepCall lat fns cfg x fn args
  Return          -> stepReturn mode lat cfg

------------------------------------------------------------------
-- Per-rule helpers
------------------------------------------------------------------

-- Pop the top PC frame on branch/loop exit.
stepResetPC :: Configuration -> Configuration
stepResetPC cfg = case cfgPCs cfg of
  (_ : pcs) -> cfg {cfgCmd = Stop, cfgPCs = pcs}
  []        -> error "PC stack underflow"

-- TODO (PC stack growth): desugaring While into If with a recursive While in
-- the then-branch means each iteration's If pushes a new PC context BEFORE
-- the previous iteration's ResetPC can fire — the outer ResetPC sits past
-- the recursive While. The stack therefore grows by one frame per iteration
-- during execution and only unwinds when the loop finally terminates,
-- costing O(iterations) memory. Correctness is unaffected; a direct While
-- rule that reuses one PC frame per iteration would fix this.
stepSeq :: ExecMode -> SecurityLattice -> [Function] -> Configuration -> Cmd -> Cmd -> Configuration
stepSeq mode lat fns cfg c1 c2 = case c1 of
  Halt -> cfg {cfgCmd = Halt}
  _ ->
    let cfg' = step mode lat fns (cfg {cfgCmd = c1})
     in case cfgCmd cfg' of
          Stop -> cfg' {cfgCmd = c2}
          Halt -> cfg' {cfgCmd = Halt}
          c1'  -> cfg' {cfgCmd = Seq c1' c2}

-- M-If: push pc ⊔ ℓ_e onto the PC stack, choose branch via memory at the
-- raised view. Under DynamicPU the deferred PU check fires here: if any
-- variable read by `e` is in P, abort.
stepIf :: ExecMode -> SecurityLattice -> Configuration -> Expr -> Cmd -> Cmd -> Configuration
stepIf mode lat cfg@Configuration {..} e c1 c2 =
  case puBranchViolation mode (varsExpr e) cfgPartials of
    Left msg -> error msg
    Right () ->
      let pc       = getPCLevel cfgPCs
          pcVars   = getPCVars cfgPCs
          bottom   = head (latticeLevels lat)
          l_e      = getExprLevel e cfgLabs bottom
          newPc    = pc \/ l_e
          newPcVrs = Set.union pcVars (varsExpr e)
          taken    = exprEval e (getMem cfgMem newPc) /= 0
          next     = if taken then Seq c1 ResetPC else Seq c2 ResetPC
       in cfg {cfgCmd = next, cfgPCs = (newPc, newPcVrs) : cfgPCs}

-- M-Assign: NSU check `pc ⊑ Γ(x)`, then write at level pc ⊔ ℓ_e and update
-- labels and influences. The conditional cleanup keeps x in dependency sets
-- iff the new value transitively depends on the old x (see comment block).
stepAssign :: ExecMode -> SecurityLattice -> Configuration -> VarName -> Expr -> Configuration
stepAssign mode lat cfg@Configuration {..} x e =
  let pc       = getPCLevel cfgPCs
      pcVars   = getPCVars cfgPCs
      bottom   = head (latticeLevels lat)
      l_e      = getExprLevel e cfgLabs bottom
      l_target = pc \/ l_e
      v        = exprEval e (getMem cfgMem l_target)
      curLab   = cfgLabs x
      carriers = Set.union (varsExpr e) pcVars
   in case applyNsu mode pc curLab x carriers cfgPartials "assignment" of
        Left msg -> error msg
        Right newP ->
          let newMem    = updateMultiMemory lat cfgMem x v l_target
              newLabs y = if y == x then l_target else cfgLabs y
              -- Drop x from other variables' deps only when the new x is
              -- independent of the old x (i.e. x ∉ closure of fv(e)). For
              -- a reflexive update like `x := x + 1`, x is in the closure,
              -- so we keep the old links.
              preClosure       = inflClosure (varsExpr e) cfgInfl
              newDependsOnOld  = Set.member x preClosure
              cleaned          =
                if newDependsOnOld
                  then cfgInfl
                  else Map.map (Set.delete x) cfgInfl
              -- Eager closure: keep dependency chains intact across
              -- intermediate reassignments.
              depsClosure = inflClosure (varsExpr e) cleaned
              newInfl     = Map.insert x (Set.union depsClosure pcVars) cleaned
           in cfg {cfgCmd = Stop, cfgMem = newMem, cfgLabs = newLabs, cfgInfl = newInfl, cfgPartials = newP}

-- M-Input: side-channel check `pc ⊑ ch`, then NSU check `pc ⊑ Γ(x)`. New
-- x is taken from the head of the input tape. The new x is independent of
-- the old x unless we're inside a branch whose condition mentions x.
stepInput :: ExecMode -> SecurityLattice -> Configuration -> Level -> VarName -> Configuration
stepInput mode lat cfg@Configuration {..} ch x =
  let pc        = getPCLevel cfgPCs
      pcVars    = getPCVars cfgPCs
      l_target  = ch \/ pc
      monitorOn = mode == DynamicNSU || mode == DynamicPU
   in -- The side-channel check is a flow check on the channel itself, not
      -- an NSU check; PU does NOT relax it.
      if monitorOn && not (pc <= ch)
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
          Right newP -> case cfgInput of
            [] ->
              error $
                "Runtime error: input tape exhausted at input to "
                  ++ x
                  ++ " on channel "
                  ++ show ch
                  ++ "."
            (v : vs) ->
              let newMem    = updateMultiMemory lat cfgMem x v l_target
                  newLabs y = if y == x then l_target else cfgLabs y
                  newDependsOnOld = Set.member x pcVars
                  cleaned   =
                    if newDependsOnOld
                      then cfgInfl
                      else Map.map (Set.delete x) cfgInfl
                  newInfl   = Map.insert x pcVars cleaned
               in cfg {cfgCmd = Stop, cfgMem = newMem, cfgLabs = newLabs, cfgInput = vs, cfgInfl = newInfl, cfgPartials = newP}

-- M-Output: flow check (ℓ_e ⊔ pc) ⊑ ch. The memory view chosen is M#ch
-- rather than M#(pc ⊔ ℓ_e); legal because ch dominates the target view
-- when the flow check passes, and the read never fires when it fails.
stepOutput :: ExecMode -> SecurityLattice -> Configuration -> Level -> Expr -> Configuration
stepOutput mode lat cfg@Configuration {..} ch e =
  let v       = exprEval e (getMem cfgMem ch)
      bottom  = head (latticeLevels lat)
      l_e     = getExprLevel e cfgLabs bottom
      pc      = getPCLevel cfgPCs
      monitorOn = mode == DynamicNSU || mode == DynamicPU
   in if monitorOn && not ((l_e \/ pc) <= ch)
        then
          error $
            "Dynamic Monitor Exception: information flow violation at output to channel "
              ++ show ch
              ++ ". (ℓ_e="
              ++ show l_e
              ++ ") ⊔ (PC="
              ++ show pc
              ++ ") ⋢ "
              ++ show ch
              ++ "."
        else cfg {cfgCmd = Stop, cfgOutput = cfgOutput ++ [(ch, v)]}

-- M-Erase: NSU check `pc ⊑ Γ(x)`, then for every dependent v of x (forward
-- closure through the influence map) zero v at views ⋢ (ℓ_cmd ⊔ Γ(v) ⊔ pc)
-- and raise its label. pc_vars are *unioned* into deps, not assigned, so a
-- later erase still finds the same dependents.
--
-- Under DynamicPU the NSU check is deferred: if `pc ⋢ Γ(x)`, the erase is
-- allowed but every dependent is added to P (its post-erase memory state at
-- low views reflects an upgrade decision). P also propagates from x itself
-- (if x was already P-marked) to its dependents.
stepErase :: ExecMode -> SecurityLattice -> Configuration -> Level -> VarName -> Configuration
stepErase mode lat cfg@Configuration {..} l_cmd x =
  let pc      = getPCLevel cfgPCs
      l_var   = cfgLabs x
      nsuFail = not (pc <= l_var)
   in if mode == DynamicNSU && nsuFail
        then
          error $
            "Dynamic Monitor Exception: No-Sensitive-Upgrade violation at erase of "
              ++ x
              ++ ". PC ("
              ++ show pc
              ++ ") ⋢ Γ("
              ++ x
              ++ ") = "
              ++ show l_var
              ++ "; a conditional erase under a higher PC would be observable at lower levels."
        else
          let pcVars = getPCVars cfgPCs
              deps   = getDependents x cfgInfl
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

-- M-Call: build the callee's memory and labels, save the caller frame
-- (including the maps needed to translate callee-side names back at
-- Return), and push the body. Non-parameter locals start at the caller's
-- pc rather than ⊥, matching PLAS'09 [U-REF].
stepCall :: SecurityLattice -> [Function] -> Configuration -> VarName -> String -> [Expr] -> Configuration
stepCall lat fns cfg@Configuration {..} x fName args =
  case filter (\f -> funcName f == fName) fns of
    [] -> error $ "Function " ++ fName ++ " not found"
    (f : _) ->
      let pc       = getPCLevel cfgPCs
          pcVars   = getPCVars cfgPCs
          bottom   = head (latticeLevels lat)
          l_args   = map (\e -> getExprLevel e cfgLabs bottom) args
          argTgts  = map (pc \/) l_args
          vals     = zipWith (\e l_t -> exprEval e (getMem cfgMem l_t)) args argTgts
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
            { cfgCmd   = Seq (funcBody f) Return
            , cfgMem   = newMem
            , cfgLabs  = newLabs
            , cfgPCs   = [(pc, pcVars)]
            , cfgStack = frame : cfgStack
            , cfgInfl  = Map.empty
            }

-- M-Return: NSU check at the receiver, then write the return value into
-- the caller's `x`. Resolves the callee-internal influence closure back
-- into caller-side names via `argToVars` (see Approach B comment).
--
-- Under DynamicPU the NSU check is deferred via applyNsu, using
-- `resolved ∪ callerPcVars` as the carriers of P. Callee-introduced names
-- in P are dropped at the boundary — they have no meaning in the caller's
-- namespace.
stepReturn :: ExecMode -> SecurityLattice -> Configuration -> Configuration
stepReturn mode lat cfg@Configuration {..} = case cfgStack of
  [] -> error "Return with empty call stack"
  (x, callerMem, callerLabs, callerPCs, retExpr, callerInfl, calleeIntro, argToVars) : restStack ->
    let pc            = getPCLevel cfgPCs
        pcVars        = getPCVars cfgPCs
        callerPC      = getPCLevel callerPCs
        callerPcVars  = getPCVars callerPCs
        bottom        = head (latticeLevels lat)
        l_ret         = getExprLevel retExpr cfgLabs bottom
        l_target      = l_ret \/ pc \/ callerPC
        v             = exprEval retExpr (getMem cfgMem l_target)
        retSeed       = Set.union (varsExpr retExpr) pcVars
        calleeTotal   = inflClosure retSeed cfgInfl
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
        carriers = Set.union resolved callerPcVars
     in case applyNsu mode callerPC (callerLabs x) x carriers cleanedP "function return" of
          Left msg -> error msg
          Right newP ->
            let finalMem    = updateMultiMemory lat callerMem x v l_target
                finalLabs y = if y == x then l_target else callerLabs y
                newXDeps    = Set.union resolved callerPcVars
                newDependsOnOld = Set.member x newXDeps
                cleanedCaller =
                  if newDependsOnOld
                    then callerInfl
                    else Map.map (Set.delete x) callerInfl
                newCallerInfl = Map.insert x newXDeps cleanedCaller
             in cfg
                  { cfgCmd      = Stop
                  , cfgMem      = finalMem
                  , cfgLabs     = finalLabs
                  , cfgPCs      = callerPCs
                  , cfgStack    = restStack
                  , cfgInfl     = newCallerInfl
                  , cfgPartials = newP
                  }

-- Shared error formatter for NSU violations.
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

-- Decide an NSU-style write site (Assign / Input / Return). Encapsulates the
-- entire NSU-vs-PU policy difference for single-target writes:
--   * DynamicNSU + `pc ⋢ Γ(x)` → Left (abort message).
--   * DynamicPU + same       → Right (P ∪ {x}): allow, mark x as partial-leak.
--   * Otherwise               → Right (P updated by propagation from rhsCarriers).
-- `rhsCarriers` is the set of variables that fed the value about to be
-- written to x (e.g. fv(e) ∪ pc_vars). When any of them is currently in P,
-- x inherits the P flag; on a clean write under a flowing PC, x is removed
-- from P (its old upgraded value is gone).
applyNsu
  :: ExecMode
  -> Level
  -> Level
  -> VarName
  -> Set.Set VarName
  -> Partials
  -> String
  -> Either String Partials
applyNsu mode pc curLab x rhsCarriers p site =
  let nsuFail   = not (pc <= curLab)
      pFromRhs  = any (`Set.member` p) (Set.toList rhsCarriers)
      addToP    = nsuFail || pFromRhs
      p'        = if addToP then Set.insert x p else Set.delete x p
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

-- INFRASTRUCTURE

data Result = Finished MultiMemory Labels [(Level, Value)] Influences Partials | OutOfFuel

evalF :: Integer -> ExecMode -> SecurityLattice -> [Function] -> Configuration -> Result
evalF 0 _ _ _ _ = OutOfFuel
evalF n mode lat fns cfg =
  let cfg' = step mode lat fns cfg
      done = Finished (cfgMem cfg') (cfgLabs cfg') (cfgOutput cfg') (cfgInfl cfg') (cfgPartials cfg')
   in case cfgCmd cfg' of
        Halt                        -> done -- Halt terminates regardless of stack depth
        Stop | null (cfgStack cfg') -> done
        _                           -> evalF (n - 1) mode lat fns cfg'

