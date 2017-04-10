{-# LANGUAGE GADTs #-}
module Ast where

type Name = String

data AExp where
  NUM :: Int -> AExp
  LOC :: Name -> AExp
  ADD :: AExp -> AExp -> AExp
  SUB :: AExp -> AExp -> AExp
  MUL :: AExp -> AExp -> AExp
  deriving (Show)

data BExp where
  TRUE  :: BExp
  FALSE :: BExp
  EQUAL :: BExp -> BExp -> BExp
  LEQ   :: BExp -> BExp -> BExp
  NOT   :: BExp -> BExp
  AND   :: BExp -> BExp -> BExp
  OR    :: BExp -> BExp -> BExp
  deriving (Show)

data Command where
  SKIP   :: Command
  ASSIGN :: Name -> AExp -> Command
  SEQ    :: Command -> Command -> Command
  IF     :: BExp -> Command -> Command -> Command
  WHILE  :: BExp -> Command -> Command
  deriving (Show)
