module Transf.AntiPattern where

-- todo:
-- - for assignables-go-right
--   - support joined conditionals on "&&", "||", etc
-- - more anti-pattern correctors

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "assignables-go-right" -:- ftype -?-
  "\"if ($x == true)\" -> \"if (true == $x)\" etc"
  -=- (\ [] -> lexPass $ assignablesGoRight)]

exprIsLRVal :: Expr -> Bool
exprIsLRVal (ExprRVal (RValLRVal _)) = True
exprIsLRVal _ = False

exprLRValToRight :: Expr -> Transformed Expr
exprLRValToRight (ExprBinOp op e1 w e2)
  | op `elem` [BEQ, BNE, BID, BNI] = swapIfGood op
  | op == BLT = swapIfGood BGT
  | op == BGT = swapIfGood BLT
  | op == BLE = swapIfGood BGE
  | op == BGE = swapIfGood BLE
  | otherwise = transfNothing
  where
  swapIfGood op' = if exprIsLRVal e1 && not (exprIsLRVal e2)
    then pure $ ExprBinOp op' e2 w e1
    else transfNothing
exprLRValToRight _ = transfNothing

assignablesGoRight :: Ast -> Transformed Ast
assignablesGoRight = modAll . modIfBlockExpr $ modWSCap2 exprLRValToRight

