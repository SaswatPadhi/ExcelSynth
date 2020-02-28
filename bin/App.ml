open Core
open Csv

open ExcelSynth
open Utils

let config_flags =
  let open Command.Let_syntax in
  [%map_open
    let aggregate_2d                = flag "enable-2d-aggregation"
                                           (optional_with_default Driver.Config.default.aggregate_2d bool)
                                           ~doc:"BOOLEAN use 2D ranges in aggregation operations"
    and col_pointwise               = flag "check-pointwise-col-operations"
                                           (optional_with_default Driver.Config.default.col_pointwise bool)
                                           ~doc:"BOOLEAN synthesize pointwise transformations for columns"
    and cost_limit                  = flag "maximum-expression-cost"
                                           (optional_with_default Driver.Config.default._Synthesizer.cost_limit int)
                                           ~doc:"INTEGER maximum cost (AST size) of expressions to explore"
    and disable_constant_solutions  = flag "disable-constant-solutions"
                                           (optional_with_default Driver.Config.default._Synthesizer.disable_constant_solutions bool)
                                           ~doc:"BOOLEAN disable constant formulas (e.g. =0.0) for cells"
    and last_col_aggregate          = flag "check-last-col-aggregations"
                                           (optional_with_default Driver.Config.default.last_col_aggregate bool)
                                           ~doc:"BOOLEAN synthesize aggregation formulas for cells in the last column"
    and last_row_aggregate          = flag "check-last-row-aggregations"
                                           (optional_with_default Driver.Config.default.last_row_aggregate bool)
                                           ~doc:"BOOLEAN synthesize aggregation formulas for cells in the last row"
    and max_threads                 = flag "max-threads"
                                           (optional_with_default Driver.Config.default.max_threads int)
                                           ~doc:"INTEGER maximum number of threads to create"
    and row_pointwise               = flag "check-pointwise-row-operations"
                                           (optional_with_default Driver.Config.default.row_pointwise bool)
                                           ~doc:"BOOLEAN synthesize pointwise transformations for rows"
    and top_left_only               = flag "restrict-to-top-left-data"
                                           (optional_with_default Driver.Config.default.top_left_only bool)
                                           ~doc:"BOOLEAN only use data to the top left of a cell in formulas"
    and type_error_threshold        = flag "type-error-threshold"
                                           (optional_with_default Driver.Config.default._Synthesizer.type_error_threshold float)
                                           ~doc:"FLOAT maximum fraction of cells that may be ignored due to type errors"
     in {
       Driver.Config.default with
       aggregate_2d ;
       col_pointwise ;
       last_col_aggregate ;
       last_row_aggregate ;
       max_threads ;
       row_pointwise ;
       top_left_only ;
       _Synthesizer = {
         Driver.Config.default._Synthesizer with
         cost_limit ;
         disable_constant_solutions ;
         type_error_threshold ;
       } ;
     } [@warning "-23"]
  ]

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary: "Synthesize Excel formulas for a CSV file."
    [%map_open
      let constants = flag "constant" (listed string)
                      ~doc:"FLOAT additional Boolean/numeric/string constants"
      and log_path  = flag "log-path" (optional string)
                      ~doc:"FILENAME enable logging and output to the specified path"
      and mask_path = flag "mask-path" (optional string)
                      ~doc:"FILENAME a known formula mask for the CSV file"
      and range     = flag "range" (optional string)
                      ~doc:"STRING a range (in RC:R'C' format) that bounds the synthesis space"
      and config    = config_flags
      and csv_path  = anon ("filename" %: string)
      in fun () ->
           Log.enable ~msg:"ExcelSynth" log_path ;
           let constants = List.map constants ~f:Value.of_string in
           let csv = Csv.load ~fix:false csv_path in
           let data = Array.(of_list_map csv ~f:(of_list_map ~f:Value.of_string)) in
           let mask = Option.map mask_path ~f:(fun p -> Csv.(to_array (load p))) in
           let data = Matrix.Offsetted.(
             match range with None -> create data
                            | Some range -> let tl , br = Excel.Range.to_rc_ints range
                                             in create data ~top_left:(Some tl) ~bottom_right:(Some br))
            in
           let res = Driver.run ~config { constants ; data ; mask }
            in Csv.(print (of_array res))
    ]

let () =
  Command.run command