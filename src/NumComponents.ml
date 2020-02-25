open Base

open Expr

let pos_div x y = (x - (x % y)) / y

let equality = [
  {
    name = "num-eq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.(equal x y));
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
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.(x >= y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ ">=" ^ b ^ ")")
  } ;
  {
    name = "num-leq";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.(x <= y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "<=" ^ b ^ ")")
  } ;
  {
    name = "num-lt";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.(x < y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "<" ^ b ^ ")")
  } ;
  {
    name = "num-gt";
    codomain = Type.BOOL;
    domain = Type.[NUM; NUM];
    can_apply = (function
                 | [x ; y] -> (x =/= y) && (not (is_constant x && is_constant y))
                 | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Bool Float.(x > y));
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
                      -> if x then (Num y) else (Num z));
    to_string = (fun [@warning "-8"] [a ; b ; c] -> "IF(" ^ a ^ "," ^ b ^ "," ^ c ^ ")")
  }
]

let octagons = intervals @ [
  {
    name = "num-add";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; Application (comp, [_ ; y])]
                         when String.equal comp.name "num-sub"
                         -> x =/= y && (x =/= Constant (Num 0.))
                       | [Application (comp, [_ ; x]) ; y]
                         when String.equal comp.name "num-sub"
                         -> x =/= y && (y =/= Constant (Num 0.))
                       | [x ; y] -> (x =/= Constant (Num 0.)) && (y =/= Constant (Num 0.))
                       | _ -> false);
    evaluate = Value.(fun [@warning "-8"] [Num x ; Num y] -> Num (x +. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "+" ^ b ^ ")")
  } ;
  {
    name = "num-sub";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [(Application (comp, [x ; y])) ; z]
                         when String.equal comp.name "num-add"
                         -> x =/= z && y =/= z && (z =/= Constant (Num 0.))
                       | [(Application (comp, [x ; _])) ; y]
                         when String.equal comp.name "num-sub"
                         -> x =/= y && (y =/= Constant (Num 0.))
                       | [x ; (Application (comp, [y ; _]))]
                         when (String.equal comp.name "num-sub" || String.equal comp.name "num-add")
                         -> x =/= y
                       | [x ; y] -> (x =/= y)
                                 && (x =/= Constant (Num 0.)) && (y =/= Constant (Num 0.))
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
                      [Num x ; Num y] when Float.(y <> 0.)
                      -> Num (x /. y));
    to_string = (fun [@warning "-8"] [a ; b] -> "(" ^ a ^ "/" ^ b ^ ")")
  } ;
  {
    name = "num-quotient";
    codomain = Type.NUM;
    domain = Type.[NUM; NUM];
    can_apply = Value.(function
                       | [x ; y] -> x =/= y
                                 && (x =/= Constant (Num 0.)) && (x =/= Constant (Num 1.))
                                 && (y =/= Constant (Num 0.)) && (y =/= Constant (Num 1.))
                       | _ -> false);
    evaluate = Value.(function [@warning "-8"]
                      [Num x ; Num y] when Float.(y <> 0.) && Caml.Float.((is_integer x) && (is_integer y))
                      -> Num (Float.of_int (pos_div (Float.to_int x) (Float.to_int x))));
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
                      [Num x ; Num y] when Float.(y <> 0.) && Caml.Float.((is_integer x) && (is_integer y))
                      -> Num (Float.of_int ((Float.to_int x) % (Float.to_int y))));
    to_string = (fun [@warning "-8"] [a ; b] -> "MOD(" ^ a ^ "," ^ b ^ ")")
  }
]

let linear_levels = [| equality ; intervals ; octagons ; polyhedra |]
let non_linear_levels = [| equality ; intervals ; octagons ; polyhedra ; polynomials ; peano |]