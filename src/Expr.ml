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
    && (try List.iter2_exn l1 l2 ~f:(fun x y -> if not (equal x y) then raise Caml.Exit) ; true
        with _ -> false)
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
  | Application (comp, args) -> comp.to_string (List.map ~f:(to_string arg_names) args)
  | Constant v -> Value.to_string v
  | Variable i -> arg_names.(i)

let rec size = function
  | Application (_, args) -> List.fold ~f:(+) ~init:1 (List.map ~f:size args)
  | _ -> 1

let rec evaluate inputs expr =
  match expr with
  | Application (comp, args) -> comp.evaluate (List.map args ~f:(evaluate inputs))
  | Constant c -> c
  | Variable i -> List.nth_exn inputs i

let rec extract_variables = function
  | Variable i -> [i]
  | Application (_, args) -> List.fold ~f:(@) ~init:[] (List.map ~f:extract_variables args)
  | _ -> []

let rec extract_applications = function
  | Application (component, args) -> List.fold ~f:(@) ~init:[component.name] (List.map ~f:extract_applications args)
  | _ -> []