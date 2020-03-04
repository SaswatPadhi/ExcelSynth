open Base

open Expr
open Utils

let quotient x y = (x - (Caml.Int.rem x y)) / y

let equality = [
  {
    name = "num-eq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.Approx.(equal x y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "=" ^ b ^ ")")
  }
]

let intervals = equality @ [
   {
    name = "num-geq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool (Float.Approx.compare x y >= 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ ">=" ^ b ^ ")")
  } ;
  {
    name = "num-leq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool (Float.Approx.compare x y <= 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "<=" ^ b ^ ")")
  } ;
  {
    name = "num-lt";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool (Float.Approx.compare x y < 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "<" ^ b ^ ")")
  } ;
  {
    name = "num-gt";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool (Float.Approx.compare x y > 0));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ ">" ^ b ^ ")")
  } ;
  {
    name = "num-ite";
    codomain = Type.NUM;
    domain = Type.[BOOL; NUM; NUM];
    can_apply = (function
                 | [x ; y ; z] -> (not (is_constant x)) && (y =/= z)
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Bool x ; Num y ; Num z]
                      -> Num (if x then y else z));
    to_string = (fun [@warning "-8"] [a ; b ; c] -> "IF(" ^ a ^ "," ^ b ^ "," ^ c ^ ")")
  }
]

let octagons = intervals @ [
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
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Num (x +. y));
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
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Num (x -. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "-" ^ b ^ ")")
  }
]

let polyhedra = octagons @ [
  {
    name = "num-lin-mult";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y]
                         -> (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.))
                         && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.))
                         && (is_constant x || is_constant y)
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Num (x *. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "*" ^ b ^ ")")
  }
]

let polynomials = polyhedra @ [
  {
    name = "num-nonlin-mult";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> not (is_constant x || is_constant y)
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Num (x *. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "*" ^ b ^ ")")
  }
]

let peano = polynomials @ [
  {
    name = "num-div";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.))
                                 && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.))
                       | _ -> false);
    evaluate = Value.(function [@warning "-8"]
                      [Num x ; Num y] when not Float.Approx.(equal y 0.)
                      -> Num (x /. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "/" ^ b ^ ")")
  } ;
  (* {
    name = "num-quotient";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.))
                                 && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.))
                       | _ -> false);
    evaluate = Value.(function [@warning "-8"]
                      [Num x ; Num y] when not Float.Approx.(equal y 0.) && Caml.Float.((is_integer x) && (is_integer y))
                      -> Num Float.(of_int (quotient (to_int x) (to_int x))));
    to_string = (fun [@warning "-8"] [a ; b] -> "QUOTIENT(" ^ a ^ "," ^ b ^ ")")
  } ;
  {
    name = "num-mod";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.))
                                 && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.))
                       | _ -> false);
    evaluate = Value.(function [@warning "-8"]
                      [Num x ; Num y] when not Float.Approx.(equal y 0.) && Caml.Float.((is_integer x) && (is_integer y))
                      -> Num Float.(of_int (Caml.Int.rem (to_int x) (to_int y))));
    to_string = (fun [@warning "-8"] [a ; b] -> "MOD(" ^ a ^ "," ^ b ^ ")")
  } *)
]

let levels = [| equality ; intervals ; octagons ; polyhedra ; polynomials ; peano |]

let no_bool_levels =
  let cequal = fun c1 c2 -> String.equal c1.name c2.name
   in Array.map [| octagons ; polyhedra ; polynomials ; peano |]
                ~f:List.(filter ~f:(fun c -> not (mem intervals c ~equal:cequal)))