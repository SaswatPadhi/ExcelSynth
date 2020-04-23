open Core_kernel

open Exceptions
open Utils

(* TODO: Think about better strategies for combining grammar levels across multiple theories.
         Given levels x = {Gx_1 ⊆ ... Gx_n} and y = {Gy_1 ⊆ ... Gy_n}, such that m > n,
         currently I pad y with Gy_n at the end such that |x| = |y| and take a pairwise union. *)

let rec ( ++ ) = fun x y ->
  let open Array in
  if length y > length x
  then map ~f:(fun (ex,ey) -> ex @ ey)
           (zip_exn (append x (create (last x) ~len:(length y - length x))) y)
  else map ~f:(fun (ex,ey) -> ex @ ey)
           (zip_exn x (append y (create (last y) ~len:(length x - length y))))

let ( -*- ) (from : Expr.component list) (comps : Expr.component list) =
  List.(fold comps ~init:from ~f:(fun from c -> filter from ~f:(fun cf -> not (String.equal c.name cf.name))))

module Config = struct
  type t = {
    components_per_level : Expr.component list array ;
    abort_on_constant_solutions : bool ;
    disable_constant_solutions : bool ;
    large_constant_inference : bool ;
    max_expressiveness_level : int ;
    order : int -> int -> float ;
    size_limit : int ;
    type_mismatch_threshold : float ;
    value_mismatch_threshold : float ;
  }

  let default : t = {
    components_per_level = NumComponents.no_bool_levels ++ RangeComponents.levels ;
    abort_on_constant_solutions = true ;
    disable_constant_solutions = true ;
    large_constant_inference = true ;
    max_expressiveness_level = 1024 ;
    order = (fun g_cost e_cost -> (Int.to_float e_cost) *. (Float.log (Int.to_float g_cost))) ;
    size_limit = 9 ;
    type_mismatch_threshold = 0.333333 ;
    value_mismatch_threshold = 0.05 ;
  }
end

type task = {
  inputs : Value.t array list ;
  outputs : Value.t array ;
  constants : Value.t list ;
}

exception Success of Expr.synthesized
exception IgnoredSolution of Expr.synthesized

module DList = Doubly_linked

let divide_size applier arity op_level expr_level remaining_size =
  let rec eq_helper arity remaining_size acc =
    if arity = 1 then
      for l = 0 to expr_level do
        applier ((l,remaining_size) :: acc)
      done
    else
      begin
        for l = 0 to expr_level do
          for s = 1 to remaining_size do
            eq_helper (arity - 1) (remaining_size - s) ((l,s) :: acc)
          done
        done
      end
  and neq_helper arity remaining_size acc =
    if arity = 1 then
      if List.exists acc ~f:(fun (l,_) -> l = expr_level) then
        begin
          for l = 0 to expr_level do
            applier ((l,remaining_size) :: acc)
          done
        end
      else
        applier ((expr_level,remaining_size) :: acc)
    else
      begin
        for l = 0 to expr_level do
          for s = 1 to remaining_size do
            neq_helper (arity - 1) (remaining_size - s) ((l,s) :: acc)
          done
        done
      end
   in if expr_level = op_level
      then eq_helper arity remaining_size []
      else neq_helper arity remaining_size []

module Output = struct
  module T = struct
    type t = Value.t array [@@deriving sexp]
    let compare (o1 : t) (o2 : t) : int =
      Array.compare Value.compare o1 o2
  end

  include T
  include Comparable.Make (T)
end

module IntTuple = struct
  module T = struct
    type t = int * int [@@deriving sexp]
    let compare ((i1a, i1b) : t) ((i2a, i2b) : t) : int =
      match Int.compare i1a i2a with
      | 0 -> Int.compare i1b i2b
      | c -> c
  end

  include T
  include Comparable.Make (T)
end

let solve_impl (config : Config.t) (task : task) =
  let typed_components t_type = Array.append
    (Array.create ~len:1 [])
    (Array.mapi (Array.init (Int.min config.max_expressiveness_level (Array.length config.components_per_level))
                            ~f:(fun i -> config.components_per_level.(i)))
                ~f:(fun level comps
                    -> List.filter ~f:(fun c -> Type.equal c.codomain t_type)
                                   (if level < 1 then comps
                                    else comps -*- (config.components_per_level.(level - 1))))) in

  let num_components = typed_components Type.NUM in
  let bool_components = typed_components Type.BOOL in
  let string_components = typed_components Type.STRING in
  let range_components = typed_components Type.RANGE in

  let empty_candidates () = Array.(init ((length config.components_per_level) + 1)
                                        ~f:(fun _ -> init config.size_limit ~f:(fun _ -> DList.create ())))
   in

  let num_candidates = empty_candidates () in
  let bool_candidates = empty_candidates () in
  let string_candidates = empty_candidates () in
  let range_candidates = empty_candidates () in

  let [@warning "-8"] typed_candidates = function
    | Type.NUM    -> num_candidates
    | Type.BOOL   -> bool_candidates
    | Type.STRING -> string_candidates
    | Type.RANGE  -> range_candidates
   in

  let seen_outputs = ref (Set.empty (module Output)) in
  let add_candidate candidates_set level cost (candidate : Expr.synthesized) =
    let old_size = Set.length !seen_outputs
     in seen_outputs := Set.add !seen_outputs candidate.outputs
      ; if (Set.length !seen_outputs) <> old_size
        then ignore (DList.insert_last candidates_set.(level).(cost) candidate)
   in

  let make_constant_candidate value : Expr.synthesized = {
    expr = Expr.Constant value;
    outputs = Array.create ~len:(Array.length task.outputs) value;
  } in

  let constants = Value.(
    List.dedup_and_sort ~compare
       ( Value.[ Num 0. ; Num 1. ; Bool true ; Bool false ]
       @ (List.map task.constants ~f:(function Num x -> Num (Float.abs x) | x -> x))))
   in

  List.(iter (rev constants) ~f:(fun c -> add_candidate (typed_candidates (Value.typeof c)) 0 1 (make_constant_candidate c))) ;

  List.iteri task.inputs
             ~f:(fun i input -> try add_candidate (typed_candidates (Value.majority_type input)) 0 1
                                                  { expr = Expr.Variable i ; outputs = input }
                                with NoMajorityType -> Log.debug (lazy ("     `-- Ignoring input v"
                                                                       ^ (Int.to_string i)
                                                                       ^ ": No majority type found!")))
  ;

  let typed_value_equal = Value.(fun v1 v2 -> not (Type.equal (typeof v2) (typeof v1)) || equal v1 v2) in
  let rec check ?(large_constant_inference = true) (candidate : Expr.synthesized) =
    let length = ref 0. in
    let mismatches = Array.fold2_exn task.outputs candidate.outputs ~init:0.
                                     ~f:(fun acc v1 v2 -> length := !length +. 1.
                                                        ; if typed_value_equal v1 v2 then acc else acc +. 1.)
     in Log.debug (lazy ("     %-- value mismatches = " ^ Float.(to_string mismatches) ^
                         "/" ^ Float.(to_string !length) ^ " :"))
      ; begin
          if Float.(mismatches <= config.value_mismatch_threshold *. !length)
          then (
            if config.disable_constant_solutions && Expr.is_constant candidate.expr
            then (
              if config.abort_on_constant_solutions then raise (IgnoredSolution candidate)
            ) else raise (Success candidate)
          )
        end
      ; begin
          if large_constant_inference
          then let apply = Expr.apply ~type_mismatch_threshold:config.type_mismatch_threshold in
               let is_trivial f = Caml.Float.is_integer f && Float.(abs f < 5.) in
               let rec process index =
                 if index < Array.length candidate.outputs && index < Array.length task.outputs then
                 match candidate.outputs.(index), task.outputs.(index) with
                 | Value.Num i , Value.Num o
                   -> begin
                        if not (is_trivial (i -. o)) then
                        let diff_candidate = make_constant_candidate (Value.Num (i -. o)) in
                        let new_candidate = apply NumComponents.subtraction [ candidate ; diff_candidate ]
                         in Option.iter new_candidate ~f:(check ~large_constant_inference:false)
                      end
                    ; begin
                        if not (Float.equal o 0.) && not (is_trivial (i /. o)) then
                        let ratio_candidate = make_constant_candidate (Value.Num (i /. o)) in
                        let new_candidate = apply NumComponents.division [ candidate ; ratio_candidate ]
                         in Option.iter new_candidate ~f:(check ~large_constant_inference:false)
                      end
                 | _ , _ -> process (index + 1)
                in process 0
        end
   in
  let check = check ~large_constant_inference:config.large_constant_inference in

  let task_codomain = Value.majority_type task.outputs in

  DList.iter ~f:check (typed_candidates task_codomain).(0).(1) ;

  let apply_component op_level expr_level cost domain applier =
    let rec apply_cells acc domain locations =
      match domain , locations with
      | typ :: arg_types , (lvl,loc) :: locations
        -> DList.iter (typed_candidates typ).(lvl).(loc)
                      ~f:(fun x -> apply_cells (x :: acc) arg_types locations)
      | ([], []) -> applier (List.rev acc)
      | _ -> raise (Internal_Exn "Impossible case!")
     in divide_size (apply_cells [] domain) (List.length domain) op_level expr_level (cost - 1)
   in
  let expand_component op_level expr_level cost candidates (component : Expr.component) =
    let applier args =
      match Expr.apply ~type_mismatch_threshold:config.type_mismatch_threshold component args with
      | None -> ()
      | Some result
        -> let expr_cost = Expr.size result.expr
            in (if expr_cost < config.size_limit && Type.equal task_codomain component.codomain then check result)
             ; add_candidate candidates expr_level expr_cost result
     in apply_component op_level expr_level cost component.domain applier
   in
  let ordered_level_cost =
    let grammar_cost level = (List.length constants) * (List.length config.components_per_level.(level-1))
     in List.sort ~compare:(fun (level1,cost1) (level2,cost2)
                           -> Float.compare (config.order (grammar_cost level1) cost1)
                                            (config.order (grammar_cost level2) cost2))
                  (List.(cartesian_product (range 1 ~stop:`inclusive (Int.min config.max_expressiveness_level
                                                                              (Array.length config.components_per_level)))
                                           (range 2 config.size_limit)))
   in

  Log.debug (lazy ( "  > Exploration order:")) ;
  Log.debug (lazy ("    " ^ (List.to_string_map ordered_level_cost ~sep:" > "
                                                ~f:(fun (l,c) -> "(G" ^ (Int.to_string l)
                                                               ^ "," ^ (Int.to_string c) ^ ")"))));
  Log.debug (lazy ("  > Checking expressions:")) ;

  let seen_level_cost = ref (Set.empty (module IntTuple)) in
  List.iter ordered_level_cost
    ~f:(fun (level,cost)
        -> List.(iter (cartesian_product (range ~stop:`inclusive 1 level) (range 2 cost))
             ~f:(fun (l,c) -> if not (Set.mem !seen_level_cost (l,c))
                              then failwith ( "Internal Error :: Not a well order!\n"
                                            ^ "Attempted to explore (G" ^ (Int.to_string level)
                                            ^ "." ^ (Int.to_string cost) ^ ") before (G"
                                            ^ (Int.to_string l) ^ "." ^ (Int.to_string c) ^ ")")))
         ; seen_level_cost := (Set.add !seen_level_cost (level, cost))
         ; List.(iter (range 1 ~stop:`inclusive level)
                      ~f:(fun l -> List.iter
                                     Type.[ (bool_candidates,   bool_components.(l))
                                          ; (num_candidates,    num_components.(l))
                                          ; (string_candidates, string_components.(l))
                                          ; (range_candidates,  range_components.(l)) ]
                                     ~f:(fun (cands, comps)
                                           -> iter comps ~f:(expand_component l level cost cands)))))

let solve ?(config = Config.default) (task : task) : Expr.synthesized =
  Log.debug (lazy ("Starting hybrid enumeration:")) ;
  Log.debug (lazy ("  > Output:")) ;
  Log.debug (lazy ("    [ " ^ (Array.to_string_map task.outputs ~sep:" ; " ~f:Value.to_string) ^ " ]")) ;
  Log.debug (lazy ("  > Inputs:")) ;
  List.(iteri task.inputs
              ~f:(fun i input -> Log.debug (lazy ("    + v" ^ (Int.to_string i) ^ ": [ " ^
                                                  (Array.to_string_map input ~sep:" ; " ~f:Value.to_string) ^ " ]")))) ;
  try solve_impl config task
    ; Log.debug (lazy ("  # NO SOLUTION FOUND!"))
    ; raise NoSuchFunction
  with NoMajorityType
       -> Log.debug (lazy ("  # NO MAJORITY OUTPUT TYPE FOUND, ABORTING!"))
        ; raise NoSuchFunction
     | IgnoredSolution candidate
       -> Log.debug (lazy ("  # Ignoring solution (@ size " ^ (Int.to_string (Expr.size candidate.expr)) ^ "):"))
        ; Log.debug (lazy ("    " ^ (Expr.to_string (Array.of_list_mapi task.inputs ~f:(fun i _ -> "v" ^ (Int.to_string i)))
                                                    candidate.expr)))
        ; raise NoSuchFunction
     | Success candidate
       -> Log.debug (lazy ("  $ Solution (@ size " ^ (Int.to_string (Expr.size candidate.expr)) ^ "):"))
        ; Log.debug (lazy ("    " ^ (Expr.to_string (Array.of_list_mapi task.inputs ~f:(fun i _ -> "v" ^ (Int.to_string i)))
                                                    candidate.expr)))
        ; candidate