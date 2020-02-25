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

let rec to_function = function
  | Application (comp, comp_args)
    -> let arg_funcs = List.map ~f:to_function comp_args
        in (fun args -> comp.evaluate (List.map arg_funcs ~f:(fun afunc -> afunc args)))
  | Constant v -> (fun _ -> v)
  | Variable i -> (fun args -> List.nth_exn args i)

let rec height = function
  | Application (_, args) -> 1 + (List.fold_left ~f:max ~init:0 (List.map ~f:height args))
  | _ -> 1

let rec size = function
  | Application (_, args) -> List.fold_left ~f:(+) ~init:1 (List.map ~f:size args)
  | _ -> 1

type synthesized = {
  expr : t ;
  outputs : Value.t array ;
} [@@deriving sexp]

let apply (comp : component) (args : synthesized list) : synthesized option =
  if (not (comp.can_apply (List.map args ~f:(fun arg -> arg.expr)))) then None
  else try
    let select idx = List.map args ~f:(fun arg -> arg.outputs.(idx))
     in Some { expr = Application (comp, List.map ~f:(fun arg -> arg.expr) args)
             ; outputs = Array.mapi (List.hd_exn args).outputs
                                    ~f:(fun i _ -> comp.evaluate (select i)) }
  with Internal_Exn _ as e -> raise e
     | _ -> None