type var = string

type aexp =
  | Num : int -> aexp
  | Var : var -> aexp
  | Add : aexp * aexp -> aexp
  | Sub : aexp * aexp -> aexp
  | Mul : aexp * aexp -> aexp

type bexp =
  | True  : bexp
  | False : bexp
  | Equal : bexp * bexp -> bexp
  | Leq   : bexp * bexp -> bexp
  | Not   : bexp -> bexp
  | And   : bexp * bexp -> bexp
  | Or    : bexp * bexp -> bexp

type command =
  | Skip   : command
  | Assign : var * aexp -> command
  | Seq    : command * command -> command
  | If     : bexp * command * command -> command
  | While  : bexp * command -> command

