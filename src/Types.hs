module Types where

import Imp
import Data.List (isSuffixOf)
import qualified Data.Map as Map

-- Standard levels
public = L "public"
secret = L "secret"

-- Lattice representation
-- For simplicity, we define the partial order and join explicitly
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

-- Lattice operations using the provided lattice
flowsto :: Lattice -> Level -> Level -> Bool
flowsto lat a b = (a, b) `elem` flows lat

join :: Lattice -> Level -> Level -> Level
join lat a b = case Map.lookup (a, b) (joins lat) of
    Just l -> l
    Nothing -> error $ "Join not defined for " ++ show a ++ " and " ++ show b

-- convenient synonyms (implicitly using stdLattice)
(⊔) :: Level -> Level -> Level
(⊔) = join stdLattice

(⊑) :: Level -> Level -> Bool
(⊑) = flowsto stdLattice

-- Flow-sensitive environment
type Environment = VarName -> Level

-- update environment
updateEnv :: Environment -> VarName -> Level -> Environment
updateEnv env x l y = if y == x then l else env y

-- join two environments
joinEnv :: Environment -> Environment -> Environment
joinEnv env1 env2 x = join stdLattice (env1 x) (env2 x)

-- check if one environment is more restrictive than another (env1 ⊑ env2)
envFlowsTo :: [VarName] -> Environment -> Environment -> Bool
envFlowsTo vars env1 env2 = all (\x -> flowsto stdLattice (env1 x) (env2 x)) vars


-- Expression Typing
exprType :: Environment -> Expr -> Level
exprType _   (IntExpr _ ) = public
exprType env (VarExpr x ) = env x
exprType env (BinOpExpr _ e1 e2) =
    join stdLattice (exprType env e1) (exprType env e2)

-- Type result now returns the new environment
data TypeRes = WellTyped Environment | TypeError String

-- Command Typing (Flow-sensitive)
cmdType :: [VarName] -> Environment -> Level -> Cmd -> TypeRes

cmdType _ env _ Skip = WellTyped env

cmdType _ env pc (Assign x e) =
    let l = exprType env e
        l' = pc ⊔ l
    in WellTyped (updateEnv env x l')

cmdType vars env pc (Seq c1 c2) =
    case cmdType vars env pc c1 of
        WellTyped env' -> cmdType vars env' pc c2
        err            -> err

cmdType vars env pc (If e c1 c2) =
    let l = exprType env e
        pc' = pc ⊔ l
    in case cmdType vars env pc' c1 of
        WellTyped env1' -> case cmdType vars env pc' c2 of
            WellTyped env2' -> WellTyped (joinEnv env1' env2')
            err -> err
        err -> err

cmdType vars env pc (While e c) =
    let l = exprType env e
        pc' = pc ⊔ l
    in case cmdType vars env pc' c of
        WellTyped env' ->
            if envFlowsTo vars env' env -- Γ' ⊑ Γ
               then WellTyped env
               else TypeError "While loop body changes environment unpredictably"
        err -> err

cmdType vars env pc (Input ch x) =
    if not (pc ⊑ ch)
    then TypeError $ "Input failed: pc (" ++ show pc ++ ") does not flow to channel (" ++ show ch ++ ")"
    else WellTyped (updateEnv env x (ch ⊔ pc))

cmdType vars env pc (Output ch e) =
    let l = exprType env e
    in if not ((pc ⊔ l) ⊑ ch)
    then TypeError $ "Output failed: pc ⊔ expr (" ++ show (pc ⊔ l) ++ ") does not flow to channel (" ++ show ch ++ ")"
    else WellTyped env

cmdType _ env _ Stop = WellTyped env


-- EXAMPLES

levelFromName :: VarName -> Level
levelFromName x =
   if "_p" `isSuffixOf` x
        then public
        else secret

allSecretEnv :: Environment
allSecretEnv _ = secret

initEnv :: [VarName] -> Environment
initEnv vars =
  foldl (\env var -> updateEnv env var (levelFromName var)) allSecretEnv vars
