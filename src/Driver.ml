open Core

open Exceptions
open Utils

module Config = struct
  type t = {
    aggregate_2d : bool ;
    col_pointwise : bool ;
    last_col_aggregate : bool ;
    last_row_aggregate : bool ;
    row_pointwise : bool ;
    top_left_only : bool ;
    _Synthesizer : Synthesizer.Config.t ;
  }

  let default : t = {
    aggregate_2d = false ;
    col_pointwise = true ;
    last_col_aggregate = false ;
    last_row_aggregate = true ;
    row_pointwise = false ;
    top_left_only = true ;
    _Synthesizer = Synthesizer.Config.default ;
  }
end

type 'a matrix = 'a array array

type task = {
  data : Value.t matrix ;
  constants : Value.t list
}

let make_pointwise_row_problem ~(config : Config.t) ~(col_mask : bool array option)
                               (task : task) (r : int) : Synthesizer.task =
  let num_args = if config.top_left_only then r else ((Array.length task.data) - 1) in
  let constants = task.constants
   in match col_mask with
  | None -> {
      constants ; num_args ;
      inputs = List.rev (
        Array.foldi task.data ~init:[]
                    ~f:(fun i acc a -> if (i = r) || (config.top_left_only && i > r) then acc
                                       else (a :: acc))) ;
      outputs = task.data.(r) ;
    }
  | Some cm -> {
      constants ; num_args ;
      inputs = List.rev (
        Array.foldi task.data ~init:[]
                    ~f:(fun i acc a -> if (i = r) || (config.top_left_only && i > r) then acc
                                       else (Array.filteri a ~f:(fun j _ -> cm.(j)) :: acc))) ;
      outputs = Array.filteri task.data.(r) ~f:(fun i _ -> cm.(i)) ;
    }

let make_pointwise_col_problem ~(config : Config.t) ~(row_mask : bool array option)
                               (task : task) (c : int) : Synthesizer.task =
  let num_args = if config.top_left_only then c else ((Array.length task.data.(0)) - 1) in
  let constants = task.constants
   in match row_mask with
      | None -> {
          constants ; num_args ;
          inputs = List.rev (
            Array.(foldi task.data.(0) ~init:[]
                         ~f:(fun i acc _ -> if (i = c) || (config.top_left_only && i > c) then acc
                                            else ((map task.data ~f:(fun a -> a.(i))) :: acc)))) ;
          outputs = Array.map task.data ~f:(fun a -> a.(c)) ;
        }
      | Some rm -> {
          constants ; num_args ;
          inputs = List.rev (
            Array.(foldi task.data.(0) ~init:[]
                         ~f:(fun i acc _ -> if (i = c) || (config.top_left_only && i > c) then acc
                                            else ((filter_mapi task.data ~f:(fun j a -> if rm.(j) then Some a.(i) else None)) :: acc)))) ;
          outputs = Array.filter_mapi task.data ~f:(fun i a -> if rm.(i) then Some a.(c) else None) ;
        }

let make_range_problem ~(config : Config.t) (task : task) (r : int) (c : int) : Synthesizer.task =
  let constants = task.constants and outputs = [| task.data.(r).(c) |]
   in if config.top_left_only
      then {
        constants ; num_args = 2 ; outputs ;
        inputs = List.(map ~f:(fun a -> Array.map a ~f:(fun l -> Value.Range (rev (map l ~f:rev))))) [
          [|
            if config.aggregate_2d
            then Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row
                                  -> if i >= r then acc else (
                                       (foldi row ~init:[] ~f:(fun j acc col -> if j > c then acc else col :: acc)) :: acc)))
            else Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row -> if i >= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row
                                  -> if i > r then acc else (
                                       (foldi row ~init:[] ~f:(fun j acc col -> if j >= c then acc else col :: acc)) :: acc)))
            else Array.(foldi task.data.(r) ~init:[]
                              ~f:(fun i acc col -> if i >= c then acc else [col] :: acc))
          |]
        ]
      } else {
        constants ; num_args = 4 ; outputs ;
        inputs = List.(map ~f:(fun a -> Array.map a ~f:(fun l -> Value.Range (rev (map l ~f:rev))))) [
          [|
            if config.aggregate_2d
            then Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row
                                  -> if i >= r then acc else ((fold row ~init:[] ~f:(fun acc col -> col :: acc)) :: acc)))
            else Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row -> if i >= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(foldi task.data ~init:[]
                         ~f:(fun i acc row
                             -> if i <= r then acc else ((fold row ~init:[] ~f:(fun acc col -> col :: acc)) :: acc)))
            else Array.(foldi task.data ~init:[]
                              ~f:(fun i acc row -> if i <= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(fold task.data ~init:[]
                        ~f:(fun acc row
                            -> (foldi row ~init:[] ~f:(fun j acc col -> if j >= c then acc else col :: acc)) :: acc))
            else Array.(foldi task.data.(r) ~init:[]
                              ~f:(fun i acc col -> if i >= c then acc else [col] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(fold task.data ~init:[]
                        ~f:(fun acc row
                            -> (foldi row ~init:[] ~f:(fun j acc col -> if j <= c then acc else col :: acc)) :: acc))
            else Array.(foldi task.data.(r) ~init:[]
                              ~f:(fun i acc col -> if i <= c then acc else [col] :: acc))
          |]
        ]
      }

let apply_row_formula ~(config : Config.t) ~(mask : string matrix) ~(col_mask : bool array option)
                      (r : int) : (Expr.t option -> unit) =
  let rlen = Array.length mask and clen = Array.length mask.(0) in
  let row_formula (c : int) (e : Expr.t) : string =
    let cells = List.(fold (range 0 rlen) ~init:[]
                            ~f:(fun acc i -> if (i = r) || (config.top_left_only && i > r) then acc
                                            else ((Excel.to_cell_name i c) :: acc)))
      in "=" ^ (Expr.to_string (Array.of_list_rev cells) e)
   in match col_mask with
      | None -> (function None -> ()
                        | Some res -> List.(
                            iter (range 0 clen)
                                 ~f:(fun c -> mask.(r).(c) <- row_formula c res)))
      | Some cm -> (function None -> ()
                           | Some res -> List.(
                               iter (range 0 clen)
                                    ~f:(fun c -> if cm.(c) then mask.(r).(c) <- row_formula c res)))

let apply_col_formula ~(config : Config.t) ~(mask : string matrix) ~(row_mask : bool array option)
                      (c : int) : (Expr.t option -> unit) =
  let rlen = Array.length mask and clen = Array.length mask.(0) in
  let col_formula (r : int) (e : Expr.t) : string =
    let cells = List.(fold (range 0 clen) ~init:[]
                            ~f:(fun acc i -> if (i = c) || (config.top_left_only && i > c) then acc
                                            else ((Excel.to_cell_name r i) :: acc)))
      in "=" ^ (Expr.to_string (Array.of_list_rev cells) e)
   in match row_mask with
      | None -> (function None -> ()
                        | Some res -> List.(
                            iter (range 0 rlen)
                                 ~f:(fun r -> mask.(r).(c) <- col_formula r res)))
      | Some rm -> (function None -> ()
                           | Some res -> List.(
                               iter (range 0 rlen)
                                    ~f:(fun r -> if rm.(r) then mask.(r).(c) <- col_formula r res)))

let apply_range_formula ~(config : Config.t) ~(mask : string matrix)
                        (r : int) (c : int) : (Expr.t option -> unit) =
  let rlen = Array.length mask and clen = Array.length mask.(0) in
  let range_formula (e : Expr.t) : string =
    let ranges = if config.top_left_only
                 then begin
                   if config.aggregate_2d
                   then [| "A1:" ^ (Excel.to_cell_name (r-1) c)
                         ; "A1:" ^ (Excel.to_cell_name r (c-1)) |]
                   else [| (Excel.to_cell_name 0 c) ^ ":" ^ (Excel.to_cell_name (r-1) c)
                         ; (Excel.to_cell_name r 0) ^ ":" ^ (Excel.to_cell_name r (c-1)) |]
                 end
                 else begin
                   if config.aggregate_2d
                   then [| "A1:" ^ (Excel.to_cell_name (r-1) (clen-1))
                         ; (Excel.to_cell_name (r+1) 0) ^ ":" ^ (Excel.to_cell_name (rlen-1) (clen-1))
                         ; "A1:" ^ (Excel.to_cell_name (rlen-1) (c-1))
                         ; (Excel.to_cell_name 0 (c+1)) ^ ":" ^ (Excel.to_cell_name (rlen-1) (clen-1)) |]
                   else [| (Excel.to_cell_name 0 c) ^ ":" ^ (Excel.to_cell_name (r-1) c)
                         ; (Excel.to_cell_name (r+1) c) ^ ":" ^ (Excel.to_cell_name (rlen-1) c)
                         ; (Excel.to_cell_name r 0) ^ ":" ^ (Excel.to_cell_name r (c-1))
                         ; (Excel.to_cell_name r (c+1)) ^ ":" ^ (Excel.to_cell_name r (clen-1)) |]
                 end
      in "=" ^ (Expr.to_string ranges e)
   in function None -> ()
             | Some res -> mask.(r).(c) <- range_formula res

let run ?(config = Config.default) (task : task) : string matrix =
  let cols = Array.(fold task.data ~init:(length task.data.(0))
                         ~f:(fun acc a -> if acc = length a then acc else -1))
   in if cols < 0 then raise (Exceptions.Internal_Exn "Not a matrix!")
      else begin
        let rlen = Array.length task.data and clen = Array.length task.data.(0) in
        let mask = Array.init rlen ~f:(fun _ -> Array.init clen ~f:(fun _ -> "")) in
        let make_pointwise_row_problem = make_pointwise_row_problem ~config task
        and make_pointwise_col_problem = make_pointwise_col_problem ~config task
        and make_range_problem = make_range_problem ~config task in
        let apply_row_formula = apply_row_formula ~config ~mask
        and apply_col_formula = apply_col_formula ~config ~mask
        and apply_range_formula = apply_range_formula ~config ~mask in
        let solver problem =
          try Some (Synthesizer.solve ~config:config._Synthesizer problem)
          with NoSuchFunction -> None
         in
            (if config.last_row_aggregate && rlen > 2 then
              List.(iter (range 0 clen)
                         ~f:(fun c -> if String.is_empty mask.(rlen-1).(c)
                                      then apply_range_formula (rlen-1) c (solver (make_range_problem (rlen-1) c)))))
          ; (if config.last_col_aggregate && clen > 2 then
              List.(iter (range 0 rlen)
                         ~f:(fun r -> if String.is_empty mask.(r).(clen-1)
                                      then apply_range_formula r (clen-1) (solver (make_range_problem r (clen-1))))))
          ; (if config.col_pointwise
             then List.(iter (range 0 clen)
                             ~f:(fun c -> let row_mask = Some (Array.map mask ~f:(fun row -> String.is_empty row.(c)))
                                           in apply_col_formula ~row_mask c (solver (make_pointwise_col_problem ~row_mask c)))))
          ; (if config.row_pointwise
             then List.(iter (range 0 rlen)
                             ~f:(fun r -> let col_mask = Some (Array.map mask.(r) ~f:String.is_empty)
                                           in apply_row_formula ~col_mask r (solver (make_pointwise_row_problem ~col_mask r)))))
          ; mask
      end