open Base

open Exceptions
open Expr
open Utils

let light_aggregate = [
   {
    name = "range-sum";
    codomain = Type.NUM;
    domain = Type.[RANGE];
    can_apply = (function _ -> true);
    evaluate = Value.(function [@warning "-8"] [Range r]
                      when Int.(List.(fold r ~init:0 ~f:(fun acc re -> acc + (length re))) > 4)
                      -> Num (List.(fold r ~init:0.
                                         ~f:(fun acc re -> acc +. (fold re ~init:0. ~f:(fun acce (Num e) -> e +. acce))))));
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
                      when Int.(List.(fold r ~init:0 ~f:(fun acc re -> acc + (length re))) > 3)
                      -> Num ((List.(fold r ~init:0.
                                          ~f:(fun acc re -> acc +. ((fold re ~init:0. ~f:(fun acce (Num e) -> e +. acce))
                                                                    /. (Float.of_int (List.length re))))))
                              /. (Float.of_int (List.length r))));
    to_string = (fun [@warning "-8"] [a] -> "AVERAGE(" ^ a ^ ")")
  }
]

let drop_head = heavy_aggregate @ [
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

let levels = [| light_aggregate ; heavy_aggregate ; drop_head ; drop_tail |]