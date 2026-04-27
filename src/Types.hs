module Types where

import Imp
import Data.List (isSuffixOf)
import qualified Data.Map as Map

-- Standard levels
public = L "public"
secret = L "secret"

-- Lattice representation
data Lattice = Lattice {
    levels :: [Level],
    flows  :: [(Level, Level)], -- (a, b) means a ⊑ b
    joins  :: Map.Map (Level, Level) Level
}

-- Initial lattice with Public ⊑ Secret
stdLattice :: Lattice
stdLattice = Lattice {
    levels = [public, secret],
    flows  = [(public, public), (public, secret), (secret, secret)],
    joins  = Map.fromList [
        ((public, public), public),
        ((public, secret), secret),
        ((secret, public), secret),
        ((secret, secret), secret)
    ]
}

-- Lattice operations
flowsto :: Lattice -> Level -> Level -> Bool
flowsto lat a b = (a, b) `elem` flows lat

join :: Lattice -> Level -> Level -> Level
join lat a b = case Map.lookup (a, b) (joins lat) of
    Just l -> l
    Nothing -> error $ "Join not defined for " ++ show a ++ " and " ++ show b

-- convenient synonyms
(⊔) :: Level -> Level -> Level
(⊔) = join stdLattice

(⊑) :: Level -> Level -> Bool
(⊑) = flowsto stdLattice

-- Flow-sensitive environment
-- (Type Environment = VarName -> Level is already defined in Imp.hs)

updateEnv :: Environment -> VarName -> Level -> Environment
updateEnv env x l = \y -> if y == x then l else env y

joinEnv :: Environment -> Environment -> Environment
joinEnv env1 env2 = \x -> join stdLattice (env1 x) (env2 x)

envFlowsTo :: [VarName] -> Environment -> Environment -> Bool
envFlowsTo vars env1 env2 = all (\x -> flowsto stdLattice (env1 x) (env2 x)) vars

-- Expression Typing
exprType :: Environment -> Expr -> Level
exprType _   (IntExpr _ ) = public
exprType env (VarExpr x ) = env x
exprType env (BinOpExpr _ e1 e2) =
    join stdLattice (exprType env e1) (exprType env e2)

data TypeRes = WellTyped Environment | TypeError String

-- Command Typing (Flow-sensitive)
cmdType :: [Function] -> [VarName] -> Environment -> Level -> Cmd -> TypeRes

-- Summary table for functions: (FunctionName, ArgLevels, PCLevel) -> ReturnLevel
type FuncSummaries = Map.Map (String, [Level], Level) Level

-- Helper for typing with summaries
cmdType' :: [Function] -> FuncSummaries -> [VarName] -> Environment -> Level -> Cmd -> TypeRes
cmdType' fns summaries vars env pc cmd = case cmd of
    Skip -> WellTyped env
    Assign x e ->
        let l = exprType env e
            l' = pc ⊔ l
        in WellTyped (updateEnv env x l')
    Seq c1 c2 ->
        case cmdType' fns summaries vars env pc c1 of
            WellTyped env' -> cmdType' fns summaries vars env' pc c2
            err            -> err
    If e c1 c2 ->
        let l = exprType env e
            pc' = pc ⊔ l
        in case cmdType' fns summaries vars env pc' c1 of
            WellTyped env1' -> case cmdType' fns summaries vars env pc' c2 of
                WellTyped env2' -> WellTyped (joinEnv env1' env2')
                err -> err
            err -> err
    While e c ->
        let l = exprType env e
            pc' = pc ⊔ l
        in case cmdType' fns summaries vars env pc' c of
            WellTyped env' -> 
                if envFlowsTo vars env' env
                   then WellTyped env
                   else TypeError "While loop body changes environment unpredictably"
            err -> err
    Input ch x ->
        if not (pc ⊑ ch)
        then TypeError $ "Input failed: pc (" ++ show pc ++ ") does not flow to channel (" ++ show ch ++ ")"
        else WellTyped (updateEnv env x (ch ⊔ pc))
    Output ch e ->
        let l = exprType env e
        in if not ((pc ⊔ l) ⊑ ch)
        then TypeError $ "Output failed: pc ⊔ expr (" ++ show (pc ⊔ l) ++ ") does not flow to channel (" ++ show ch ++ ")"
        else WellTyped env
    Call x fName args ->
        case filter (\f -> funcName f == fName) fns of
            [] -> TypeError $ "Function " ++ fName ++ " not found"
            (f:_) ->
                let argLevels = map (exprType env) args
                in case Map.lookup (fName, argLevels, pc) summaries of
                    Just retLevel -> WellTyped (updateEnv env x (pc ⊔ retLevel))
                    Nothing -> TypeError $ "Function summary not found for " ++ fName
    Return -> WellTyped env
    Stop -> WellTyped env

-- Compute function summaries using fixed-point iteration
computeSummaries :: [Function] -> FuncSummaries
computeSummaries fns = iterateSummaries fns initialSummaries
  where
    allLevels = [public, secret]
    
    -- All possible argument level combinations for a function
    argCombos f = combinations (length (funcArgs f))
    combinations 0 = [[]]
    combinations n = [ l:ls | l <- allLevels, ls <- combinations (n-1) ]

    -- Initial assumption: all functions return public
    initialSummaries = Map.fromList [ ((funcName f, argL, pcL), public) 
                                    | f <- fns, argL <- argCombos f, pcL <- allLevels ]

    iterateSummaries fns' current =
        let next = foldl ( \acc f -> 
                foldl ( \acc' argL -> -- argL is [Level]
                    foldl ( \acc'' pcL -> -- pcL is Level
                        let initFEnv y = case lookup y (zip (funcArgs f) argL) of
                                            Just l -> l
                                            Nothing -> public
                            fVars = getVars (funcBody f)
                            -- Note: we use 'current' summaries during this step
                            res = cmdType' fns current fVars initFEnv pcL (funcBody f)
                        in case res of
                            WellTyped fEnvAfter ->
                                let retL = exprType fEnvAfter (funcReturn f)
                                in Map.insert (funcName f, argL, pcL) retL acc''
                            _ -> acc'' -- If it fails, it stays at previous level or omitted
                    ) acc' allLevels -- pcL from allLevels
                ) acc (argCombos f) -- argL from argCombos f
              ) current fns'
        in if next == current then current else iterateSummaries fns' next

-- Wrap original cmdType to use summaries
cmdType fns vars env pc cmd =
    let summaries = computeSummaries fns
    in cmdType' fns summaries vars env pc cmd


-- EXAMPLES

-- We still allow the suffix convention for initial levels, 
-- but now we also default to public if no suffix is found.
levelFromName :: VarName -> Level
levelFromName x 
   | "_p" `isSuffixOf` x = public
   | "_s" `isSuffixOf` x = secret
   | otherwise           = public -- Default to public for inferred variables

initEnv :: [VarName] -> Environment
initEnv vars =
  foldl (\env var -> updateEnv env var (levelFromName var)) (\_ -> public) vars
