open Core

open Matrix.Infix
open Utils

module Config = struct
  type t = {
    aggregate_2d : bool ;
    col_pointwise : bool ;
    last_col_aggregate : bool ;
    last_row_aggregate : bool ;
    max_aggregate_size : int ;
    max_pointwise_size : int ;
    max_threads : int ;
    row_pointwise : bool ;
    top_left_only : bool ;
    _Synthesizer : Synthesizer.Config.t ;
  }

  let default : t = {
    aggregate_2d = false ;
    col_pointwise = true ;
    last_col_aggregate = true ;
    last_row_aggregate = true ;
    max_aggregate_size = 8 ;
    max_pointwise_size = 9 ;
    max_threads = 1 ;
    row_pointwise = false ;
    top_left_only = true ;
    _Synthesizer = Synthesizer.Config.default ;
  }
end

type 'a task = {
  constants : Value.t list ;
  data : 'a Matrix.Offsetted.t ;
  mask : string Matrix.t option ;
}

let make_pointwise_row_problem ~(config : Config.t) ~(col_mask : bool array option)
                               (task : Value.t task) (r : int) : Synthesizer.task =
  let constants = task.constants
   in match col_mask with
  | None -> {
      constants ;
      inputs = List.rev (
        Array.foldi !>(task.data) ~init:[]
                    ~f:(fun i acc a -> if (i = r) || (config.top_left_only && i > r) then acc
                                       else (a :: acc))) ;
      outputs = !>(task.data).(r) ;
    }
  | Some cm -> {
      constants ;
      inputs = List.rev (
        Array.foldi !>(task.data) ~init:[]
                    ~f:(fun i acc a -> if (i = r) || (config.top_left_only && i > r) then acc
                                       else (Array.filteri a ~f:(fun j _ -> cm.(j)) :: acc))) ;
      outputs = Array.filteri !>(task.data).(r) ~f:(fun i _ -> cm.(i)) ;
    }

let make_pointwise_col_problem ~(config : Config.t) ~(row_mask : bool array option)
                               (task : Value.t task) (c : int) : Synthesizer.task =
  let constants = task.constants
   in match row_mask with
      | None -> {
          constants ;
          inputs = List.rev (
            Array.(foldi !>(task.data).(0) ~init:[]
                         ~f:(fun i acc _ -> if (i = c) || (config.top_left_only && i > c) then acc
                                            else ((map !>(task.data) ~f:(fun a -> a.(i))) :: acc)))) ;
          outputs = Array.map !>(task.data) ~f:(fun a -> a.(c)) ;
        }
      | Some rm -> {
          constants ;
          inputs = List.rev (
            Array.(foldi !>(task.data).(0) ~init:[]
                         ~f:(fun i acc _ -> if (i = c) || (config.top_left_only && i > c) then acc
                                            else ((filter_mapi !>(task.data) ~f:(fun j a -> if rm.(j) then Some a.(i) else None)) :: acc)))) ;
          outputs = Array.filter_mapi !>(task.data) ~f:(fun i a -> if rm.(i) then Some a.(c) else None) ;
        }

let make_range_problem ~(config : Config.t) (task : Value.t task) (r : int) (c : int) : Synthesizer.task =
  let constants = task.constants and outputs = [| !>(task.data).(r).(c) |]
   in if config.top_left_only
      then {
        constants ; outputs ;
        inputs = List.(map ~f:(fun a -> Array.map a ~f:(fun l -> Value.Range (rev (map l ~f:rev))))) [
          [|
            if config.aggregate_2d
            then Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row
                                  -> if i >= r then acc else (
                                       (foldi row ~init:[] ~f:(fun j acc col -> if j > c then acc else col :: acc)) :: acc)))
            else Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row -> if i >= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row
                                  -> if i > r then acc else (
                                       (foldi row ~init:[] ~f:(fun j acc col -> if j >= c then acc else col :: acc)) :: acc)))
            else Array.[foldi !>(task.data).(r) ~init:[]
                              ~f:(fun i acc col -> if i >= c then acc else col :: acc)]
          |]
        ]
      } else {
        constants ; outputs ;
        inputs = List.(map ~f:(fun a -> Array.map a ~f:(fun l -> Value.Range (rev (map l ~f:rev))))) [
          [|
            if config.aggregate_2d
            then Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row
                                  -> if i >= r then acc else ((fold row ~init:[] ~f:(fun acc col -> col :: acc)) :: acc)))
            else Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row -> if i >= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(foldi !>(task.data) ~init:[]
                         ~f:(fun i acc row
                             -> if i <= r then acc else ((fold row ~init:[] ~f:(fun acc col -> col :: acc)) :: acc)))
            else Array.(foldi !>(task.data) ~init:[]
                              ~f:(fun i acc row -> if i <= r then acc else [row.(c)] :: acc))
          |] ; [|
            if config.aggregate_2d
            then Array.(fold !>(task.data) ~init:[]
                        ~f:(fun acc row
                            -> (foldi row ~init:[] ~f:(fun j acc col -> if j >= c then acc else col :: acc)) :: acc))
            else Array.[foldi !>(task.data).(r) ~init:[]
                              ~f:(fun i acc col -> if i >= c then acc else col :: acc)]
          |] ; [|
            if config.aggregate_2d
            then Array.(fold !>(task.data) ~init:[]
                        ~f:(fun acc row
                            -> (foldi row ~init:[] ~f:(fun j acc col -> if j <= c then acc else col :: acc)) :: acc))
            else Array.[foldi !>(task.data).(r) ~init:[]
                              ~f:(fun i acc col -> if i <= c then acc else col :: acc)]
          |]
        ]
      }

let can_replace_with_formula (output : Value.t) (index : int) (formula : Synthesizer.result) : bool =
  Type.(equal NUM (Value.typeof output)) && Value.equal output formula.outputs.(index)
[@@inline always]

let apply_row_formula ~(config : Config.t) ~(data : Value.t Matrix.Offsetted.t) ~(mask : string Matrix.Offsetted.t)
                      ~(col_mask : bool array option) (r : int) : (Synthesizer.result option -> unit) =
  let ot , ol = Matrix.Offsetted.top_left mask in
  let row_formula (c : int) (e : Expr.t) : string =
    let cells = Array.(foldi !>mask ~init:[]
                             ~f:(fun i acc _ -> if (i = r) || (config.top_left_only && i > r) then acc
                                                else ((Excel.Cell.of_rc_ints (i + ot) (c + ol)) :: acc)))
      in "=" ^ (Expr.to_string (Array.of_list_rev cells) e)
   in match col_mask with
      | None -> (function
                 | None -> ()
                 | Some res -> Array.iteri !>mask.(0)
                                           ~f:(fun c _ -> if can_replace_with_formula !>data.(r).(c) c res
                                                          then !>mask.(r).(c) <- row_formula c res.expr))
      | Some cm -> (function
                    | None -> ()
                    | Some res -> let i = ref (-1)
                                   in Array.iteri !>mask.(0)
                                                  ~f:(fun c _ -> if cm.(c) && (Int.incr i ; can_replace_with_formula !>data.(r).(c) !i res)
                                                                 then !>mask.(r).(c) <- row_formula c res.expr))

let apply_col_formula ~(config : Config.t) ~(data : Value.t Matrix.Offsetted.t) ~(mask : string Matrix.Offsetted.t)
                      ~(row_mask : bool array option) (c : int) : (Synthesizer.result option -> unit) =
  let ot , ol = Matrix.Offsetted.top_left mask in
  let col_formula (r : int) (e : Expr.t) : string =
    let cells = Array.(foldi !>mask.(r) ~init:[]
                             ~f:(fun i acc _ -> if (i = c) || (config.top_left_only && i > c) then acc
                                                else ((Excel.Cell.of_rc_ints (r + ot) (i + ol)) :: acc)))
      in "=" ^ (Expr.to_string (Array.of_list_rev cells) e)
   in match row_mask with
      | None -> (function None -> ()
                        | Some res -> Array.iteri !>mask
                                                  ~f:(fun r _ -> if can_replace_with_formula !>data.(r).(c) r res
                                                                 then !>mask.(r).(c) <- col_formula r res.expr))
      | Some rm -> (function
                    | None -> ()
                    | Some res -> let i = ref (-1)
                                   in Array.iteri !>mask
                                                  ~f:(fun r _ -> if rm.(r) && (Int.incr i ; can_replace_with_formula !>data.(r).(c) !i res)
                                                                 then !>mask.(r).(c) <- col_formula r res.expr))

let apply_range_formula ~(config : Config.t) ~(data : Value.t Matrix.Offsetted.t) ~(mask : string Matrix.Offsetted.t)
                        (r : int) (c : int) : (Synthesizer.result option -> unit) =
  let ot , ol = Matrix.Offsetted.top_left mask in
  let rlen = Array.length !>mask and clen = Array.length !>mask.(0) in
  let range_formula (e : Expr.t) : string =
    let ranges = if config.top_left_only
                 then begin
                   if config.aggregate_2d
                   then [| (Excel.Cell.of_rc_ints ot     ol)     ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot-1) (c+ol))
                         ; (Excel.Cell.of_rc_ints ot     ol)     ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot)   (c+ol-1)) |]
                   else [| (Excel.Cell.of_rc_ints ot     (c+ol)) ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot-1) (c+ol))
                         ; (Excel.Cell.of_rc_ints (r+ot) ol)     ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot)   (c+ol-1)) |]
                 end
                 else begin
                   if config.aggregate_2d
                   then [| (Excel.Cell.of_rc_ints ot       ol)       ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot-1)    (clen+ol-1))
                         ; (Excel.Cell.of_rc_ints (r+ot+1) ol)       ^ ":" ^ (Excel.Cell.of_rc_ints (rlen+ot-1) (clen+ol-1))
                         ; (Excel.Cell.of_rc_ints ot       ol)       ^ ":" ^ (Excel.Cell.of_rc_ints (rlen+ot-1) (c+ol-1))
                         ; (Excel.Cell.of_rc_ints ot       (c+ol+1)) ^ ":" ^ (Excel.Cell.of_rc_ints (rlen+ot-1) (clen+ol-1)) |]
                   else [| (Excel.Cell.of_rc_ints ot       (c+ol))   ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot-1)    (c+ol))
                         ; (Excel.Cell.of_rc_ints (r+ot+1) (c+ol))   ^ ":" ^ (Excel.Cell.of_rc_ints (rlen+ot-1) (c+ol))
                         ; (Excel.Cell.of_rc_ints (r+ot)   ol)       ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot)      (c+ol-1))
                         ; (Excel.Cell.of_rc_ints (r+ot)   (c+ol+1)) ^ ":" ^ (Excel.Cell.of_rc_ints (r+ot)      (clen+ol-1)) |]
                 end
      in "=" ^ (Expr.to_string ranges e)
   in function None -> ()
             | Some res -> if can_replace_with_formula !>data.(r).(c) 0 res
                           then !>mask.(r).(c) <- range_formula res.expr

let run_on_values ?(config = Config.default) (task : Value.t task) : string Matrix.t =
  let cols = Array.(fold !!(task.data) ~init:(length !!(task.data).(0))
                         ~f:(fun acc a -> if acc = length a then acc else -1))
   in (if cols < 0 then raise (Invalid_argument "Not a matrix!"))
    ; let make_pointwise_row_problem = make_pointwise_row_problem ~config task
      and make_pointwise_col_problem = make_pointwise_col_problem ~config task
      and make_range_problem = make_range_problem ~config task in
      let rlen = Array.length !>(task.data) and clen = Array.length !>(task.data).(0) in
      let mask : string Matrix.Offsetted.t =
        match task.mask with
        | None -> Matrix.Offsetted.(create (Array.map !!(task.data) ~f:(Array.map ~f:(fun _ -> "")))
                                           ~top_left:(Some (top_left task.data))
                                           ~bottom_right:(Some (bottom_right task.data)))
        | Some mask -> if Array.((length mask) = (length !!(task.data))
                       && (for_all mask ~f:(fun re -> (length re) = (length !!(task.data).(0)))))
                       then Matrix.Offsetted.(create mask ~top_left:(Some (top_left task.data))
                                                          ~bottom_right:(Some (bottom_right task.data)))
                       else raise (Invalid_argument "Provided 'mask' dimensions do not match 'data' dimensions!")
        in
      let apply_row_formula = apply_row_formula ~config ~mask ~data:task.data
      and apply_col_formula = apply_col_formula ~config ~mask ~data:task.data
      and apply_range_formula = apply_range_formula ~config ~mask ~data:task.data
       in
      let solver problem =
        let config = {
          config._Synthesizer with
          size_limit = config.max_pointwise_size
        } in try Some (Synthesizer.solve ~config problem)
             with Exceptions.NoSuchFunction -> None
      and agg_solver problem =
        let config = {
          config._Synthesizer with
          abort_on_constant_solutions = false ;
          size_limit = config.max_aggregate_size ;
          large_constant_threshold = -1
        } in try Some (Synthesizer.solve ~config problem)
             with Exceptions.NoSuchFunction -> None
       in
      let numeric_row r =
        Array.exists !>(task.data).(r) ~f:(fun c -> Type.(equal NUM (Value.typeof c)))
       in
      let numeric_col c =
        Array.exists !>(task.data) ~f:(fun r -> Type.(equal NUM (Value.typeof r.(c))))
       in
          Lwt_preemptive.(simple_init () ; set_bounds (0, config.max_threads) ; set_max_number_of_threads_queued 1024)
        ; (if config.last_row_aggregate && rlen > 3
           then (Log.empty_line () ; Log.info (lazy "----< ROW AGGREGATES >----") ; Log.push_indent () ;
                 Lwt_main.run (
                   Lwt_list.iter_p
                     (fun last_row -> Lwt_list.iter_p
                                        (fun c -> if String.is_empty !>mask.(last_row).(c)
                                                  then let work () = apply_range_formula last_row c (agg_solver (make_range_problem last_row c))
                                                        in Lwt_preemptive.detach work ()
                                                  else Lwt.return ())
                                        (List.range 0 clen))
                     (List.(filter (range 0 rlen)
                                   ~f:(fun r -> numeric_row r && (r = rlen - 1 || not (numeric_row (r + 1))))))))
           else Log.info (lazy "ROW AGGREGATES SKIPPED!"))
        ; (if config.col_pointwise
           then (Log.empty_line () ; Log.info (lazy "----< COL POINTWISE >----") ; Log.push_indent () ;
                 Lwt_main.run (
                   Lwt_list.iter_p (fun c -> let row_mask = Some (Array.map !>mask ~f:(fun row -> String.is_empty row.(c))) in
                                             let work () = apply_col_formula ~row_mask c (solver (make_pointwise_col_problem ~row_mask c))
                                              in Lwt_preemptive.detach work ())
                                   (List.range 0 clen)) ;
                 Log.pop_indent ())
           else Log.info (lazy "COL POINTWISE SKIPPED!"))
        ; (if config.last_col_aggregate && clen > 3
           then (Log.empty_line () ; Log.info (lazy "----< COL AGGREGATES >----") ; Log.push_indent () ;
                 Lwt_main.run (
                   Lwt_list.iter_p
                     (fun last_col -> Lwt_list.iter_p
                                        (fun r -> if String.is_empty !>mask.(r).(last_col)
                                                  then let work () = apply_range_formula r last_col (agg_solver (make_range_problem r last_col))
                                                        in Lwt_preemptive.detach work ()
                                                  else Lwt.return ())
                                        (List.range 0 rlen))
                     (List.(filter (range 0 clen)
                                   ~f:(fun c -> numeric_col c && (c = clen - 1 || not (numeric_col (c + 1))))))))
           else Log.info (lazy "COL AGGREGATES SKIPPED!"))
        ; (if config.row_pointwise
           then (Log.empty_line () ; Log.info (lazy "----< ROW POINTWISE >----") ; Log.push_indent () ;
                 Lwt_main.run (
                   Lwt_list.iter_p (fun r -> let col_mask = Some (Array.map !>mask.(r) ~f:String.is_empty) in
                                             let work () = apply_row_formula ~col_mask r (solver (make_pointwise_row_problem ~col_mask r))
                                               in Lwt_preemptive.detach work ())
                                   (List.range 0 rlen)) ;
                 Log.pop_indent ())
           else Log.info (lazy "ROW POINTWISE SKIPPED!"))
        ; Matrix.Offsetted.commit mask

let run ?(config = Config.default) (task : string task) : string Matrix.t =
  let cols = Array.(fold !!(task.data) ~init:(length !!(task.data).(0))
                         ~f:(fun acc a -> if acc = length a then acc else -1))
   in if cols < 0 then raise (Invalid_argument "Not a matrix!")
      else begin
        let rlen = Array.length !!(task.data) and clen = Array.length !!(task.data).(0) in
        let ot , ol = Matrix.Offsetted.top_left task.data in
        let mask : string Matrix.t =
          match task.mask with
          | None -> Array.map !!(task.data) ~f:(Array.map ~f:(fun _ -> ""))
          | Some mask -> if Array.((length mask) = rlen && (for_all mask ~f:(fun re -> (length re) = clen)))
                         then mask
                         else raise (Invalid_argument "Provided 'mask' dimensions do not match 'data' dimensions!")
         in Array.(iteri !>(task.data) ~f:(fun i -> iteri ~f:(fun j s -> if String.is_empty s
                                                                         then mask.(ot+i).(ol+j) <- "<empty>" )))
          ; let data = Matrix.Offsetted.(create (Array.(map !!(task.data) ~f:(map ~f:Value.of_string)))
                                                ~top_left:(Some (top_left task.data))
                                                ~bottom_right:(Some (bottom_right task.data)))
             in let mask = run_on_values ~config { task with data ; mask = (Some mask) }
                 in Array.(map mask ~f:(map ~f:(fun s -> if String.equal s "<empty>" then "" else s)))
      end