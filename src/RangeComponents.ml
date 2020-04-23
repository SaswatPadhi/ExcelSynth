open Base

open Exceptions
open Expr
open Utils

let raw_vector_sum = List.fold ~init:0. ~f:(+.)

let vector_sum = List.fold ~init:0. ~f:(fun [@warning "-8"] acce (Value.Num e) -> e +. acce)

let raw_range_sum = List.fold ~init:0. ~f:(fun acc re -> acc +. (raw_vector_sum re))

let range_sum = List.fold ~init:0. ~f:(fun acc re -> acc +. (vector_sum re))

let size = List.(fold ~init:0 ~f:(fun acc re -> acc + (length re)))

let light_aggregate = [
   {
    name = "range-sum";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      when (size r) > 4
                      -> Num (range_sum r));
    to_string = (fun [@warning "-8"] [a] -> "SUM(" ^ a ^ ")")
  }
]

let heavy_aggregate = light_aggregate @ [
  {
    name = "range-avg";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      when (size r) > 3
                      -> Num ((range_sum r) /. (Float.of_int (size r))));
    to_string = (fun [@warning "-8"] [a] -> "AVERAGE(" ^ a ^ ")")
  }
]

let heavier_aggregate = heavy_aggregate @ [
  {
    name = "range-stdev";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      when (size r) > 3
                      -> let sz = Float.of_int (size r) in
                         let avg = (range_sum r) /. sz
                          in Num (Float.sqrt List.(
                            (raw_range_sum (map r (map ~f:(fun (Num n) -> (n -. avg) **. 2.)))) /. sz
                          )));
    to_string = (fun [@warning "-8"] [a] -> "STDEVP(" ^ a ^ ")")
  }
]

let drop_head = heavier_aggregate @ [
   {
    name = "range-drop-top";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] when Int.(List.length r > 1)
                      -> Range (List.drop r 1));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 1 0 0 0
                                            with _ -> "DROP_TOP(" ^ a ^ ")")
  } ;
  {
    name = "range-drop-left";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] when List.(for_all r ~f:(fun re -> Int.(length re > 1)))
                      -> Range (List.(map r ~f:(fun re -> drop re 1))));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 1 0 0
                                            with _ -> "DROP_LEFT(" ^ a ^ ")")
  }
]

let drop_tail = drop_head @ [
   {
    name = "range-drop-bottom";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r] when Int.(List.length r > 1)
                      -> Range (List.drop_last_exn r));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 0 (-1) 0
                                            with _ -> "DROP_BOTTOM(" ^ a ^ ")")
  } ;
  {
    name = "range-drop-right";
    codomain = Type.RANGE;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      when List.(for_all r ~f:(fun re -> Int.(length re > 1)))
                      -> Range (List.(map r ~f:drop_last_exn)));
    to_string = (fun [@warning "-8"] [a] -> try Excel.Range.offset a 0 0 0 (-1)
                                            with _ -> "DROP_RIGHT(" ^ a ^ ")")
  }
]

let levels = [| light_aggregate ; heavy_aggregate ; heavier_aggregate ; drop_head ; drop_tail |]