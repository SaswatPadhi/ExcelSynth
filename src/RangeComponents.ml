open Base

open Exceptions
open Expr
open Utils

let __MIN_RANGE_SIZE__ = 4

let size_check_then_aggregate r ~f =
  if (Value.Range.num_count r) >= __MIN_RANGE_SIZE__
  then Value.Num (f r)
  else raise (Invalid_argument "Range too small!")

let aggregates = [
   {
    name = "range-sum";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      -> size_check_then_aggregate r ~f:Range.sum);
    to_string = (fun [@warning "-8"] [a] -> "SUM(" ^ a ^ ")")
  }
]

let uncommon_aggregates = [
  {
    name = "range-avg";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      -> size_check_then_aggregate r ~f:Range.average);
    to_string = (fun [@warning "-8"] [a] -> "AVERAGE(" ^ a ^ ")")
  }
]

let complex_aggregates = [
  {
    name = "range-stdev";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      -> size_check_then_aggregate r ~f:Range.stdev);
    to_string = (fun [@warning "-8"] [a] -> "STDEVP(" ^ a ^ ")")
  }
]

let rare_aggregates = [
  {
    name = "range-max";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      -> size_check_then_aggregate r ~f:(fun r -> Option.value_exn (Range.max r)));
    to_string = (fun [@warning "-8"] [a] -> "MAX(" ^ a ^ ")")
  }
]

let drop_head = [
   {
    name = "range-drop-top";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] -> Range (List.drop r 1));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 1 0 0 0
                                            with _ -> "DROP_TOP(" ^ a ^ ")")
  } ;
  {
    name = "range-drop-left";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] -> Range (List.(map r ~f:(fun re -> drop re 1))));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 1 0 0
                                            with _ -> "DROP_LEFT(" ^ a ^ ")")
  }
]

let drop_tail = [
   {
    name = "range-drop-bottom";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] -> Range (List.drop_last_exn r));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 0 (-1) 0
                                            with _ -> "DROP_BOTTOM(" ^ a ^ ")")
  } ;
  {
    name = "range-drop-right";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      -> Range (List.(map r ~f:drop_last_exn)));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 0 0 (-1)
                                            with _ -> "DROP_RIGHT(" ^ a ^ ")")
  }
]

let levels = Array.accumulate_lists [|
  aggregates ;
  uncommon_aggregates ;
  complex_aggregates ;
  rare_aggregates ;
  drop_head ;
  drop_tail
|]