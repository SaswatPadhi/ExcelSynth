open Base

open Expr
open Utils

let value_of : Value.t -> float =
  function [@warning "-8"]
  | Num x -> x
  | String "" -> 0.

let translation = [
  {
    name = "num-add";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> (x =/= Constant (Num 0.)) && (y =/= Constant (Num 0.))
                                 && (match [x ; y] with
                                     | [x ; Application (comp, [_ ; y])]
                                       when String.equal comp.name "num-sub"
                                       -> x =/= y
                                     | [Application (comp, [_ ; x]) ; y]
                                       when String.equal comp.name "num-sub"
                                       -> x =/= y
                                     | _ -> true)
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Num ((value_of v1) +. (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "+" ^ b ^ ")")
  } ;
  {
    name = "num-sub";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> (x =/= y)
                                 && (x =/= Constant (Num 0.)) && (y =/= Constant (Num 0.))
                                 && (match [x ; y] with
                                     | [(Application (comp, [x ; y])) ; z]
                                       when String.equal comp.name "num-add"
                                       -> x =/= z && y =/= z
                                     | [(Application (comp, [x ; _])) ; y]
                                       when String.equal comp.name "num-sub"
                                       -> x =/= y
                                     | [x ; (Application (comp, [y ; _]))]
                                       when String.(equal comp.name "num-sub" || equal comp.name "num-add")
                                       -> x =/= y
                                     | _ -> true)
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Num ((value_of v1) -. (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "-" ^ b ^ ")")
  }
]

let scaling = [
  {
    name = "num-mult";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y]
                         -> (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.)) && (x =/= Constant (Num (-1.)))
                         && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.)) && (x =/= Constant (Num (-1.)))
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Num ((value_of v1) *. (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "*" ^ b ^ ")")
  } ;
  {
    name = "num-div";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.)) && (x =/= Constant (Num (-1.)))
                                 && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.)) && (y =/= Constant (Num (-1.)))
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Num ((value_of v1) /. (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "/" ^ b ^ ")")
  }
]

let conditionals = [
  {
    name = "num-eq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Bool Float.Approx.(equal (value_of v1) (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "=" ^ b ^ ")")
  } ;
  {
    name = "num-geq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Bool Float.Approx.(compare (value_of v1) (value_of v2) >= 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ ">=" ^ b ^ ")")
  } ;
  {
    name = "num-leq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [v1 ; v2] -> Bool Float.Approx.(compare (value_of v1) (value_of v2) <= 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "<=" ^ b ^ ")")
  } ;
  {
    name = "num-ite";
    codomain = Type.NUM;
    domain = Type.[BOOL; NUM; NUM];
    can_apply = (function
                 | [x ; y ; z] -> (not (is_constant x)) && (y =/= z)
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Bool x ; v1 ; v2]
                      -> Num (if x then (value_of v1) else (value_of v2)));
    to_string = (fun [@warning "-8"] [a ; b ; c] -> "IF(" ^ a ^ "," ^ b ^ "," ^ c ^ ")")
  }
]


let addition = List.find_exn translation ~f:(fun c -> String.equal c.name "num-add")

let subtraction = List.find_exn translation ~f:(fun c -> String.equal c.name "num-sub")

let multiplication = List.find_exn scaling ~f:(fun c -> String.equal c.name "num-mult")

let division = List.find_exn scaling ~f:(fun c -> String.equal c.name "num-div")


let levels = Array.accumulate_lists [| translation ; scaling ; conditionals |]

let no_bool_levels = Array.accumulate_lists [| translation ; scaling |]