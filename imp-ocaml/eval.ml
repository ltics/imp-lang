open Ast

type store = (string * int) list

let rec eval_aexp (st, e) : int =
  match e with
    | Num n        -> n
    | Var name     -> List.assoc name st
    | Add (e1, e2) -> eval_aexp (st, e1) + eval_aexp (st, e2)
    | Sub (e1, e2) -> eval_aexp (st, e1) - eval_aexp (st, e2)
    | Mul (e1, e2) -> eval_aexp (st, e1) * eval_aexp (st, e2)

let rec eval_bexp (st, e) : bool =
  match e with
    | True           -> true
    | False          -> true
    | Equal (e1, e2) -> eval_bexp (st, e1) == eval_bexp (st, e2)
    | Leq (e1, e2)   -> eval_bexp (st, e1) <= eval_bexp (st, e2)
    | Not e          -> eval_bexp (st, e) |> not
    | And (e1, e2)   -> eval_bexp (st, e1) && eval_bexp (st, e2)
    | Or (e1, e2)    -> eval_bexp (st, e1) || eval_bexp (st, e2)

let rec eval_command (st, e) : store =
  match e with
    | Skip -> st
    | Assign (var, e) -> List.cons (var, eval_aexp (st, e)) st
    | Seq (c1, c2) ->
      let st' = eval_command (st, c1)
      in eval_command (st', c2)
    | If (p, c, a) ->
      if eval_bexp (st, p)
      then eval_command (st, c)
      else eval_command (st, a)
    | While (p, c) ->
      if eval_bexp (st, p)
      then eval_command (st, Seq(c, e))
      else st
