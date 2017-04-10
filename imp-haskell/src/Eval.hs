module Eval where

import Ast
import Data.Map as M

type Store = M.Map String Int

evalAExp :: Store -> AExp -> Maybe Int
evalAExp _ (NUM val) = Just val
evalAExp st (LOC name) = M.lookup name st
evalAExp st (ADD e1 e2) = (+) <$> evalAExp st e1 <*> evalAExp st e2
evalAExp st (SUB e1 e2) = (-) <$> evalAExp st e1 <*> evalAExp st e2
evalAExp st (MUL e1 e2) = (*) <$> evalAExp st e1 <*> evalAExp st e2

evalBExp :: Store -> BExp -> Maybe Bool
evalBExp _ TRUE = Just True
evalBExp _ FALSE = Just False
evalBExp st (EQUAL e1 e2) = (==) <$> evalBExp st e1 <*> evalBExp st e2
evalBExp st (LEQ e1 e2) = (<=) <$> evalBExp st e1 <*> evalBExp st e2
evalBExp st (NOT e) = not <$> evalBExp st e
evalBExp st (AND e1 e2) = (&&) <$> evalBExp st e1 <*> evalBExp st e2
evalBExp st (OR e1 e2) = (||) <$> evalBExp st e1 <*> evalBExp st e2

evalCommand :: Store -> Command -> Maybe Store
evalCommand st SKIP = Just st
evalCommand st (ASSIGN name e) = (\v -> M.insert name v st) <$> evalAExp st e
evalCommand st (SEQ c1 c2) = do
  st' <- evalCommand st c1
  evalCommand st' c2
evalCommand st (IF p c a) = do
  pv <- evalBExp st p
  if pv
    then evalCommand st c
    else evalCommand st a
evalCommand st w@(WHILE p body) = do
  pv <- evalBExp st p
  if pv
    then evalCommand st (SEQ body w)
    else return st
