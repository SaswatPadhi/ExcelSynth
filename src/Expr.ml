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

type synthesized = {
  expr : t ;
  outputs : Value.t array ;
} [@@deriving sexp]

let apply ?(type_mismatch_threshold = 0.0) (comp : component) (args : synthesized list) : synthesized option =
  let subexprs = List.map args ~f:(fun arg -> arg.expr)
   in if not (comp.can_apply subexprs) then None
      else try
        let select idx = List.map args ~f:(fun arg -> arg.outputs.(idx)) in
        let mismatches = ref 0. and length = ref 0. in
        let outputs = Array.mapi (List.hd_exn args).outputs
                                 ~f:(fun i _ -> try length := !length +. 1.
                                                  ; comp.evaluate (select i)
                                                with Match_failure _ -> mismatches := !mismatches +. 1.
                                                                      ; Value.Error)
         in if Float.(!mismatches > type_mismatch_threshold *. !length) then None
            else begin
              let expr = Application (comp, subexprs)
               in Log.debug (lazy ("    @ type mismatches = " ^ Float.(to_string !mismatches)
                                  ^ "/" ^ Float.(to_string !length) ^ " :"))
                ; Log.debug (lazy ("    + " ^ (to_string (Array.of_list_map (List.range 0 1024)
                                                                            ~f:(fun i -> "v" ^ (Int.to_string i)))
                                                         expr)))
                ; Log.debug (lazy ("     `-- [| " ^ Array.(to_string_map outputs ~sep:" ; " ~f:Value.to_string) ^ " |]"))
                ; Some { expr ; outputs }
            end
      with Internal_Exn _ as e -> raise e
         | e -> None