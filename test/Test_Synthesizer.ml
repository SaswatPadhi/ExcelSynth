open Base

open ExcelSynth

open Synthesizer

let plus_x_y () =
  let result = solve {
    inputs = List.map ~f:(Array.map ~f:(fun i -> Value.Num i))
               [ [| 3. ; 7. ; -1. ; -4. |]
               ; [| 3. ; 2. ; 13. ; 11. |] ];
    outputs = Array.map ~f:(fun i -> Value.Num i) [| 6. ; 9. ; 12. ; 7. |];
    constants = []
  } in
  let result_string = Expr.to_string [| "x" ; "y" |] result.expr
   in Alcotest.(check string) "identical" "(x+y)" result_string

let ge_plus_x_z_y () =
  let result = solve {
    inputs = List.map ~f:(Array.map ~f:(fun i -> Value.Num i))
               [ [| 3. ;   7. ;  -1. ; -4. ;  6. |]
               ; [| 9. ;  -3. ; -10. ; 11. ; -1. |]
               ; [| 7. ; -20. ; -50. ; 11. ; -1. |] ];
    outputs = Array.map ~f:(fun b -> Value.Bool b)
                        [| true ; false ; false ; false ; true |];
    constants = []
  } in
  let result_string = Expr.to_string [| "x" ; "y" ; "z" |] result.expr
   in Alcotest.(check string) "identical" "((x+z)>=y)" result_string

let not_or_eq_w_x_eq_y_z () =
  let result = solve ~config:{ Config.default with components_per_level = BooleanComponents.levels ++ NumComponents.linear_levels } {
    inputs = List.map ~f:(Array.map ~f:(fun i -> Value.Num i))
               [ [| 4. ; -1. ;  -5. ;  1. ;  -1. ; 20. |]
               ; [| 3. ;  7. ;  -1. ; -4. ;   1. ; 20. |]
               ; [| 9. ; -3. ; -10. ; 11. ; -10. ; 20. |]
               ; [| 1. ; -6. ; -10. ; 11. ;  -1. ; -3. |] ];
    outputs = Array.map ~f:(fun b -> Value.Bool b)
                        [| true ; true ; false ; false ; true ; false |];
    constants = []
  } in
  let result_string = Expr.to_string [| "w" ; "x" ; "y" ; "z" |] result.expr
   in Alcotest.(check string) "identical" "NOT(OR((w=x),(y=z)))" result_string

let all = [
  "(+ x y)",                         `Quick, plus_x_y ;
  "(>= (+ x z) y)",                  `Quick, ge_plus_x_z_y ;
  "(not (= (= w x) (= y z)))",       `Quick, not_or_eq_w_x_eq_y_z ;
]