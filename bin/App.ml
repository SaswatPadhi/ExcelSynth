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
    and arg_type_mismatch_threshold = flag "argtype-error-threshold"
                                           (optional_with_default Driver.Config.default._Synthesizer.arg_type_mismatch_threshold float)
                                           ~doc:"FLOAT maximum fraction of cells that may be ignored due to arg-type errors"
    and last_col_aggregate          = flag "check-last-col-aggregations"
                                           (optional_with_default Driver.Config.default.last_col_aggregate bool)
                                           ~doc:"BOOLEAN synthesize aggregation formulas for cells in the last column"
    and last_row_aggregate          = flag "check-last-row-aggregations"
                                           (optional_with_default Driver.Config.default.last_row_aggregate bool)
                                           ~doc:"BOOLEAN synthesize aggregation formulas for cells in the last row"
    and col_pointwise               = flag "check-pointwise-col-operations"
                                           (optional_with_default Driver.Config.default.col_pointwise bool)
                                           ~doc:"BOOLEAN synthesize pointwise transformations for columns"
    and crop_empty_border           = flag "crop-empty-border"
                                           (optional_with_default Driver.Config.default.crop_empty_border bool)
                                           ~doc:"BOOLEAN crop out empty rows and columns around the given range"
    and row_pointwise               = flag "check-pointwise-row-operations"
                                           (optional_with_default Driver.Config.default.row_pointwise bool)
                                           ~doc:"BOOLEAN synthesize pointwise transformations for rows"
    and disable_constant_solutions  = flag "disable-constant-solutions"
                                           (optional_with_default Driver.Config.default._Synthesizer.disable_constant_solutions bool)
                                           ~doc:"BOOLEAN disable constant formulas (e.g. =0.0) for cells"
    and disable_booleans             = flag "disable-booleans"
                                           (optional_with_default false bool)
                                           ~doc:"BOOLEAN disable Booleans and conditional expressions"
    and large_constant_threshold    = flag "large-constant-threshold"
                                           (optional_with_default Driver.Config.default._Synthesizer.large_constant_threshold int)
                                           ~doc:"INTEGER threshold for identifying large constants (-1 to disable)"
    and max_aggregate_size          = flag "max-aggregate-expr-size"
                                           (optional_with_default Driver.Config.default.max_aggregate_size int)
                                           ~doc:"INTEGER maximum AST size for aggregation formulas"
    and max_pointwise_size          = flag "max-pointwise-expr-size"
                                           (optional_with_default Driver.Config.default.max_pointwise_size int)
                                           ~doc:"INTEGER maximum AST size for pointwise transformations"
    and max_variable_occurrence     = flag "max-variable-occurrence"
                                           (optional_with_default Driver.Config.default._Synthesizer.max_variable_occurrence int)
                                           ~doc:"INTEGER maximum occurrences of a variable within an expression"
    and max_aggregation_occurrence  = flag "max-aggregation-occurrence"
                                           (optional_with_default Driver.Config.default._Synthesizer.max_aggregation_occurrence int)
                                           ~doc:"INTEGER maximum occurrences of aggregation operators within an expression"
    and max_threads                 = flag "max-threads"
                                           (optional_with_default Driver.Config.default.max_threads int)
                                           ~doc:"INTEGER maximum number of threads to create"
    and relative_error              = flag "relative-error"
                                           (optional_with_default !Float.Approx.rel_error float)
                                           ~doc:"FLOAT the fractional relative error allowed in float comparisons"
    and top_left_only               = flag "restrict-to-top-left-data"
                                           (optional_with_default Driver.Config.default.top_left_only bool)
                                           ~doc:"BOOLEAN only use data to the top left of a cell in formulas"
    and value_mismatch_threshold    = flag "value-error-threshold"
                                           (optional_with_default Driver.Config.default._Synthesizer.value_mismatch_threshold float)
                                           ~doc:"FLOAT maximum fraction of cells that may be ignored due to value errors"
     in Float.Approx.rel_error := relative_error
      ; let components_per_level = Synthesizer.(
          if disable_booleans
          then NumComponents.no_bool_levels ++ RangeComponents.levels
          else BooleanComponents.levels ++ NumComponents.levels ++ RangeComponents.levels
        ) in {
          Driver.Config.default with
          aggregate_2d ;
          col_pointwise ;
          crop_empty_border ;
          last_col_aggregate ;
          last_row_aggregate ;
          max_aggregate_size ;
          max_pointwise_size ;
          max_threads ;
          row_pointwise ;
          top_left_only ;
          _Synthesizer = {
            Driver.Config.default._Synthesizer with
            arg_type_mismatch_threshold ;
            components_per_level ;
            disable_constant_solutions ;
            large_constant_threshold ;
            max_variable_occurrence ;
            max_aggregation_occurrence ;
            value_mismatch_threshold ;
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
           let data = Array.(of_list_map csv ~f:(of_list_map ~f:(String.strip))) in
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