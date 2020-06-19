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
    large_constant_threshold : int ;
    max_expressiveness_level : int ;
    max_variable_occurrence : int ;
    max_aggregation_occurrence : int ;
    order : int -> int -> float ;
    size_limit : int ;
    arg_type_mismatch_threshold : float ;
    value_mismatch_threshold : float ;
  }

  let default : t = {
    components_per_level = NumComponents.levels ++ RangeComponents.levels ;
    abort_on_constant_solutions = true ;
    disable_constant_solutions = true ;
    large_constant_threshold = 5 ;
    max_expressiveness_level = 1024 ;
    max_variable_occurrence = 2 ;
    max_aggregation_occurrence = 1 ;
    order = (fun g_size e_size -> (Int.to_float e_size) *. (Float.log (Int.to_float g_size))) ;
    size_limit = 9 ;
    arg_type_mismatch_threshold = 0.6 ;
    value_mismatch_threshold = 0.05 ;
  }
end

type task = {
  inputs : Value.t array list ;
  outputs : Value.t array ;
  constants : Value.t list ;
}

type result = {
  expr : Expr.t ;
  outputs : Value.t array ;
} [@@deriving sexp]

exception Success of result
exception IgnoredSolution of result

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

let __MAX_VARIABLES__ = 128

let var_occurrence_bad ~(config : Config.t) (expr : Expr.t) : bool =
  let var_frequencies = List.group ~break:(<>) (Expr.extract_variables expr)
   in List.(exists var_frequencies
                   ~f:(fun l -> length l > config.max_variable_occurrence))

(* 
let app_occurrence_bad ~(config : Config.t) (expr : Expr.t) : bool =
  let agg_app_count = List.(fold (Expr.extract_applications expr) ~init:0
                                 ~f:(fun acc name -> if exists (Array.last RangeComponents.aggregation_levels)
                                                               ~f:(fun (c : Expr.component) -> String.equal c.name name)
                                                     then acc + 1 else acc))
   in agg_app_count > config.max_aggregation_occurrence
 *)

let app_occurrence_bad ~(config : Config.t) (expr : Expr.t) : bool =
  let app_frequencies = List.group ~break:String.(<>) (Expr.extract_applications expr)
   in List.(exists app_frequencies
                   ~f:(fun l -> length l > config.max_aggregation_occurrence
                             && let name = hd_exn l
                                 in Array.exists RangeComponents.levels
                                                 ~f:(exists ~f:(fun (c : Expr.component) -> String.equal c.name name))))

let create_candidate ~(config : Config.t) (comp : Expr.component) (args : result list) : result option =
  let subexprs = List.map args ~f:(fun arg -> arg.expr)
   in if not (comp.can_apply subexprs) then None
      else let expr = Expr.Application (comp, subexprs)
            in if var_occurrence_bad ~config expr || app_occurrence_bad ~config expr then None
               else try
                 let outputs =
                   Array.mapi (List.hd_exn args).outputs
                              ~f:(fun i _ -> try comp.evaluate (List.map args ~f:(fun arg -> arg.outputs.(i)))
                                             with Match_failure _ -> Value.Error)
                  in Some { expr ; outputs }
               with Internal_Exn _ as e -> raise e
                  | _ -> None

let check_candidate_args ~(config : Config.t) (candidate : result) : bool =
  let mismatches = ref 0. and length = ref 0.
   in Array.iteri candidate.outputs
                  ~f:(fun i v -> length := !length +. 1.
                               ; if Value.(equal v Error) then mismatches := !mismatches +. 1.)
    ; Log.debug (lazy ("    @ arg-type mismatches = " ^ Int.(to_string (of_float !mismatches))
                                  ^ " / " ^ Int.(to_string (of_float !length)) ^ " :"))
    ; Log.debug (lazy ("    + " ^ (Expr.to_string (Array.of_list_map (List.range 0 __MAX_VARIABLES__)
                                                                     ~f:(fun i -> "v" ^ (Int.to_string i)))
                                                  candidate.expr)))
    ; Log.debug (lazy ("     `-- [| " ^ Array.(to_string_map candidate.outputs ~sep:" ; " ~f:Value.to_string) ^ " |]"))
    ; Float.(!mismatches <= config.arg_type_mismatch_threshold *. !length)

let create_and_check_candidate_args ~(config : Config.t) (comp : Expr.component) (args : result list)
                                    : result option =
  match create_candidate ~config comp args with
  | None -> None
  | Some candidate -> if Float.(check_candidate_args ~config candidate)
                      then Some candidate
                      else None

let solve_impl ~(config : Config.t) (task : task) =
  let task_codomain = Value.majority_type task.outputs in
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
  let add_candidate candidates_set level size (candidate : result) =
    let old_size = Set.length !seen_outputs
     in seen_outputs := Set.add !seen_outputs candidate.outputs
      ; if (Set.length !seen_outputs) <> old_size
        then ignore (DList.insert_last candidates_set.(level).(size) candidate)
   in

  let make_constant_candidate value : result = {
    expr = Expr.Constant value;
    outputs = Array.create ~len:(Array.length task.outputs) value;
  } in

  let constants = Value.(
    List.dedup_and_sort ~compare
       ( Value.[ Num 0. ; Num 1. ; Bool true ; Bool false ]
       @ (List.map task.constants ~f:(function Num x -> Num (Float.abs x) | x -> x))))
   in

  let safe_apply = create_and_check_candidate_args ~config in
  let typed_value_equal = Value.(fun v1 v2 -> equal v2 Error || equal v1 v2) in

  let rec check ?(large_constant_inference = true) (candidate : result) =
    if Array.(length task.outputs <> length candidate.outputs) then () ;
    let length = ref 0. in
    let mismatches = Array.fold2_exn task.outputs candidate.outputs ~init:0.
                                     ~f:(fun acc v1 v2 -> length := !length +. 1.
                                                        ; if typed_value_equal v1 v2 then acc else acc +. 1.)
     in Log.debug (lazy ("         (value mismatches = " ^ Int.(to_string (of_float mismatches)) ^
                         " / " ^ Int.(to_string (of_float !length)) ^ ")"))
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
          if large_constant_inference && Expr.size candidate.expr < config.size_limit
          then let is_trivial f = (Caml.Float.is_integer f) && Float.(abs f < (of_int config.large_constant_threshold)) in
               let rec process index =
                 if index < Array.length candidate.outputs && index < Array.length task.outputs then
                 match candidate.outputs.(index), task.outputs.(index) with
                 | Value.Num i , Value.Num o
                   -> begin
                        let d = i -. o
                         in if not (is_trivial d)
                            then Option.iter ~f:(check ~large_constant_inference:false)
                                             (if Float.(d > 0.)
                                              then let diff_candidate = make_constant_candidate (Value.Num d)
                                                    in safe_apply NumComponents.subtraction [ candidate ; diff_candidate ]
                                              else let diff_candidate = make_constant_candidate (Value.Num (0. -. d))
                                                    in safe_apply NumComponents.addition [ candidate ; diff_candidate ])
                      end
                    ; begin
                        let r = i /. o
                         in if not (is_trivial r)
                            then Option.iter ~f:(check ~large_constant_inference:false)
                                             (if Float.(r > 1.)
                                              then let ratio_candidate = make_constant_candidate (Value.Num r)
                                                    in safe_apply NumComponents.division [ candidate ; ratio_candidate ]
                                              else let ratio_candidate = make_constant_candidate (Value.Num (1. /. r))
                                                    in safe_apply NumComponents.multiplication [ candidate ; ratio_candidate ])
                      end
                 | _ , _ -> process (index + 1)
                in process 0
        end
   in

  Log.debug (lazy ("  > Checking constants and identity expressions :")) ;

  List.(iter (rev constants)
             ~f:(fun c -> let candidate = make_constant_candidate c
                           in if check_candidate_args ~config candidate
                              then begin
                                add_candidate (typed_candidates (Value.typeof c)) 0 1 candidate ;
                                check ~large_constant_inference:false candidate
                              end))
  ;

  List.iteri task.inputs
             ~f:(fun i input -> let candidate = { expr = Expr.Variable i ; outputs = input }
                                 in if check_candidate_args ~config candidate
                                    then try
                                      add_candidate (typed_candidates (Value.majority_type input)) 0 1 candidate ;
                                      check ~large_constant_inference:(config.large_constant_threshold >= 0) candidate
                                    with NoMajorityType -> Log.debug (lazy ("       (ignoring input v"
                                                                           ^ (Int.to_string i)
                                                                           ^ ": no majority type found!)")))
  ;

  let check = check ~large_constant_inference:(config.large_constant_threshold >= 0) in

  let apply_component op_level expr_level size domain applier =
    let rec apply_cells acc domain locations =
      match domain , locations with
      | typ :: arg_types , (lvl,loc) :: locations
        -> DList.iter (typed_candidates typ).(lvl).(loc)
                      ~f:(fun x -> apply_cells (x :: acc) arg_types locations)
      | ([], []) -> applier (List.rev acc)
      | _ -> raise (Internal_Exn "Impossible case!")
     in divide_size (apply_cells [] domain) (List.length domain) op_level expr_level (size - 1)
   in
  let expand_component op_level expr_level size candidates (component : Expr.component) =
    let applier args =
      match safe_apply component args with
      | None -> ()
      | Some result
        -> let expr_size = Expr.size result.expr
            in (if expr_size <= config.size_limit && Type.equal task_codomain component.codomain then check result)
             ; add_candidate candidates expr_level expr_size result
     in apply_component op_level expr_level size component.domain applier
   in
  let ordered_level_size =
    let grammar_size level = (List.length constants) * (List.length config.components_per_level.(level-1))
     in List.sort ~compare:(fun (level1,size1) (level2,size2)
                           -> Float.compare (config.order (grammar_size level1) size1)
                                            (config.order (grammar_size level2) size2))
                  (List.(cartesian_product (range 1 ~stop:`inclusive (Int.min config.max_expressiveness_level
                                                                              (Array.length config.components_per_level)))
                                           (range 2 config.size_limit)))
   in

  Log.debug (lazy ( "  > Exploration order:")) ;
  Log.debug (lazy ("    " ^ (List.to_string_map ordered_level_size ~sep:" > "
                                                ~f:(fun (l,c) -> "(G" ^ (Int.to_string l)
                                                               ^ "," ^ (Int.to_string c) ^ ")"))));
  Log.debug (lazy ("  > Checking larger expressions:")) ;

  let seen_level_size = ref (Set.empty (module IntTuple)) in
  List.iter ordered_level_size
    ~f:(fun (level,size)
        -> List.(iter (cartesian_product (range ~stop:`inclusive 1 level) (range 2 size))
             ~f:(fun (l,c) -> if not (Set.mem !seen_level_size (l,c))
                              then failwith ( "Internal Error :: Not a well order!\n"
                                            ^ "Attempted to explore (G" ^ (Int.to_string level)
                                            ^ "." ^ (Int.to_string size) ^ ") before (G"
                                            ^ (Int.to_string l) ^ "." ^ (Int.to_string c) ^ ")")))
         ; seen_level_size := (Set.add !seen_level_size (level, size))
         ; List.(iter (range 1 ~stop:`inclusive level)
                      ~f:(fun l -> List.iter
                                     Type.[ (bool_candidates,   bool_components.(l))
                                          ; (num_candidates,    num_components.(l))
                                          ; (string_candidates, string_components.(l))
                                          ; (range_candidates,  range_components.(l)) ]
                                     ~f:(fun (cands, comps)
                                           -> iter comps ~f:(expand_component l level size cands)))))

let solve ?(config = Config.default) (task : task) : result =
  Log.debug (lazy "") ;
  Log.debug (lazy ("Starting hybrid enumeration:")) ;
  Log.debug (lazy ("  > Output:")) ;
  Log.debug (lazy ("    [ " ^ (Array.to_string_map task.outputs ~sep:" ; " ~f:Value.to_string) ^ " ]")) ;
  Log.debug (lazy ("  > Inputs:")) ;
  List.(iteri task.inputs
              ~f:(fun i input -> Log.debug (lazy ("    + v" ^ (Int.to_string i) ^ ": [ " ^
                                                  (Array.to_string_map input ~sep:" ; " ~f:Value.to_string) ^ " ]")))) ;
  try solve_impl ~config task
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