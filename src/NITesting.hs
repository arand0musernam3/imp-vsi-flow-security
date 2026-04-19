module NITesting where

-- Acknowledgment: Mathias Pedersen
--

import Imp


import Control.Monad
import Test.QuickCheck       -- cabal install quickcheck
import Test.QuickCheck.Monadic

instance Arbitrary BinOp
  where arbitrary = elements [ Plus, Minus, Times]

allowedIds = ["x1_p", "x2_p", "x3_p", "y1_s", "y2_s", "y3_s"]

allowedIdsGen = elements allowedIds

idExprGen = do i <- allowedIdsGen
               return (VarExpr i)


constExprGen = do k <- choose (0,4)  -- our constanst go between 0 and 4
                  return $ IntExpr k


binOpExprGen 0 = oneof [ idExprGen, constExprGen]
binOpExprGen n | n > 0  = frequency
      [ (3, idExprGen),
        (3, constExprGen),
        (2, liftM3 BinOpExpr arbitrary (binOpExprGen (n `div` 2)) ( binOpExprGen (n `div` 2) ))]


instance Arbitrary Expr where
    arbitrary = sized binOpExprGen

    shrink (BinOpExpr op e1 e2) = [e1, e2] ++ [ BinOpExpr op e1' e2 | e1' <- shrink e1]
                                           ++ [ BinOpExpr op e1 e2' | e2' <- shrink e2]

    shrink (IntExpr k) = [ IntExpr i  | i <- shrink k]

    shrink _ = []


-- Generators for statements

assignGen = do
    var <- allowedIdsGen
    exp <- arbitrary
    return $ Assign var exp


ifGen 0 = error "should not be called"
ifGen n = do
   expr <- arbitrary
   c1 <- stmtExprGen (n `div` 2)
   c2 <- stmtExprGen (n `div` 2)
   return $ If expr c1 c2

whileGen 0 = error "should not be called"
whileGen n = do
  expr <- arbitrary
  c <- stmtExprGen (n `div` 2)
  return $ While expr c

seqGen 0 = error "should not be called"
seqGen n = do
  c1 <- stmtExprGen (n `div` 2)
  c2 <- stmtExprGen (n `div` 2)
  return $ Seq c1 c2

skipGen = return Skip

stmtExprGen 0 = return Skip
stmtExprGen n | n > 0 = frequency
   [ (1, assignGen), (1, ifGen n), (1, whileGen n), (1, seqGen n), (1, skipGen)]



instance Arbitrary Cmd where
  arbitrary = sized stmtExprGen

  shrink (Assign id e) = [ Assign id e' | e' <- shrink e]
  shrink (Seq c1 c2)  = [c1, c2] ++ [ Seq c1 c2' | c2' <- shrink c2]
                                 ++ [ Seq c1' c2 | c1' <- shrink c1]
  shrink (While e c)  = [c] ++ [ While e' c | e' <- shrink e]
                            ++ [ While e c' | c' <- shrink c]

  shrink (If e c1 c2) = [c1, c2] ++ [ If e' c1 c2 | e' <- shrink e]
                                 ++ [ If e c1' c2 | c1' <- shrink c1]
                                 ++ [ If e c1 c2' | c2' <- shrink c2]
  shrink _ = []


--------------------------------------------------

-- Properties
