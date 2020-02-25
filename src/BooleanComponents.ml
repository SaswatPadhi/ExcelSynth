open Base

open Expr

let all = [
  {
    name = "not";
    codomain = Type.BOOL;
    domain = [Type.BOOL];
    can_apply = (function
                 | [Application (comp, _)] when String.equal comp.name "not"
                  -> false
                 | [ e ] -> not (is_constant e)
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Bool x] -> Bool (not x));
    to_string = (fun [@warning "-8"] [a] -> "NOT(" ^ a ^ ")")
  } ;
  {
    name = "and";
    codomain = Type.BOOL;
    domain = Type.[BOOL; BOOL];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x || is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Bool x ; Bool y] -> Bool (x && y));
    to_string = (fun [@warning "-8"] [a ; b] -> "AND(" ^ a ^ "," ^ b ^ ")")
  } ;
  {
    name = "or";
    codomain = Type.BOOL;
    domain = Type.[BOOL; BOOL];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x || is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Bool x ; Bool y] -> Bool (x || y));
    to_string = (fun [@warning "-8"] [a ; b] -> "OR(" ^ a ^ "," ^ b ^ ")")
  }
]

let levels = [| all |]