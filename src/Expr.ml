open Base

open Utils
open Exceptions

type component = {
  can_apply : t list -> bool ;
  codomain  : Type.t ;
  domain    : Type.t list ;
  evaluate  : Value.t list -> Value.t ;
  name      : string ;
  to_string : string list -> string ;
} and t =
  | Application of component * t list
  | Constant of Value.t
  | Variable of int
  [@@deriving sexp]

let rec equal e1 e2 =
  match e1, e2 with
  | Variable i1, Variable i2 -> i1 = i2
  | Constant v1, Constant v2 -> Value.equal v1 v2
  | Application (c1, l1), Application (c2, l2)
    -> String.equal c1.name c2.name
    && List.fold2_exn l1 l2 ~init:true ~f:(fun acc x y -> acc && (equal x y))
  | _ -> false

let (=/=) = fun x y -> (not (equal x y))

let is_constant expr =
  let rec helper = function
    | Constant _ -> ()
    | Variable _ -> raise Caml.Exit
    | Application (_, exprs) -> List.iter ~f:helper exprs
  in try helper expr ; true
     with Caml.Exit -> false

let rec to_string arg_names = function
  | Application (comp, comp_args) -> comp.to_string (List.map ~f:(to_string arg_names) comp_args)
  | Constant v -> Value.to_string v
  | Variable i -> arg_names.(i)

let rec size = function
  | Application (_, args) -> List.fold_left ~f:(+) ~init:1 (List.map ~f:size args)
  | _ -> 1