open Base

open ExcelSynth

open Driver
open Utils

let test_matrix m1 m2 =
  Array.iteri m1 ~f:(fun i x1 -> Alcotest.(check (array string)
                                                ("@ Row " ^ (Int.to_string (i + 1)))) x1 m2.(i))

let pointwise_col_test () =
  let config = {
    Config.default with
    last_row_aggregate = false
  } in
  let result = run_on_values ~config {
    constants = [] ;
    data = Matrix.Offsetted.create (Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|   1. ;  10. ;   9.5 ;  11. |] ;
      [|  23. ;  12. ;   0.5 ;  35. |] ;
      [|  22. ;   2. ;   -9. ;  24. |] ;
      [|  -1. ;   6. ;   6.5 ;   5. |] ;
      [|  59. ;   0. ; -29.5 ;  59. |] ;
      [|  11. ;  -2. ;  -7.5 ;   9. |] ;
      [|  92. ;   0. ;  -46. ;  92. |] ;
    |]) ;
    mask = None ;
  }
  and expected = [|
    [|              "" ;              "" ; "=(B1-(A1/(1.+1.)))" ; "=(A1+B1)" |] ;
    [|              "" ;              "" ; "=(B2-(A2/(1.+1.)))" ; "=(A2+B2)" |] ;
    [|              "" ;              "" ; "=(B3-(A3/(1.+1.)))" ; "=(A3+B3)" |] ;
    [|              "" ;              "" ; "=(B4-(A4/(1.+1.)))" ; "=(A4+B4)" |] ;
    [|              "" ;              "" ; "=(B5-(A5/(1.+1.)))" ; "=(A5+B5)" |] ;
    [|              "" ;              "" ; "=(B6-(A6/(1.+1.)))" ; "=(A6+B6)" |] ;
    [|              "" ;              "" ; "=(B7-(A7/(1.+1.)))" ; "=(A7+B7)" |] ;
  |] in test_matrix expected result

let pointwise_row_col_test () =
  let config = {
    Config.default with
    row_pointwise = true ;
    last_row_aggregate = false
  } in
  let result = run_on_values ~config {
    constants = [] ;
    data = Matrix.Offsetted.create (Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|   1. ;  10. ;   0. ;  11. |] ;
      [|  23. ;  12. ;   7. ;  35. |] ;
      [|  22. ;   2. ;   7. ;  24. |] ;
      [|  -1. ;   6. ;  23. ;   5. |] ;
      [|  58. ;   1. ;   3. ;  59. |] ;
      [|  99. ;  -8. ;  33. ;  91. |] ;
      [| 179. ;  -5. ;  43. ; 174. |] ;
    |]) ;
    mask = None ;
  }
  and expected = [|
    [|              "" ;              "" ;              "" ; "=(A1+B1)" |] ;
    [|              "" ;              "" ;              "" ; "=(A2+B2)" |] ;
    [|      "=(A2-A1)" ;      "=(B2-B1)" ;      "=(C2-C1)" ; "=(A3+B3)" |] ;
    [|              "" ;              "" ;              "" ; "=(A4+B4)" |] ;
    [|              "" ;              "" ;              "" ; "=(A5+B5)" |] ;
    [|              "" ;              "" ;              "" ; "=(A6+B6)" |] ;
    [| "=((A3+A5)+A6)" ; "=((B3+B5)+B6)" ; "=((C3+C5)+C6)" ; "=(A7+B7)" |] ;
  |] in test_matrix expected result

let last_row_aggregate_test () =
  let result = run_on_values {
    constants = [] ;
    data = Matrix.Offsetted.create (Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|   1. ;  10. ;   9.5 ;  24. |] ;
      [|  23. ;  12. ;   0.5 ;  35. |] ;
      [|  22. ;   2. ;   -9. ;  24. |] ;
      [|  -1. ;   6. ;   6.5 ;   5. |] ;
      [|  59. ;   0. ; -29.5 ;  41. |] ;
      [|  11. ;  -2. ;  -7.5 ;   9. |] ;
      [| 115. ;  14. ; -43.5 ;  23. |] ;
    |]) ;
    mask = None ;
  }
  and expected = [|
    [|            "" ;                      "" ; "=(B1-(A1/(1.+1.)))" ;                "" |] ;
    [|            "" ;                      "" ; "=(B2-(A2/(1.+1.)))" ;                "" |] ;
    [|            "" ;                      "" ; "=(B3-(A3/(1.+1.)))" ;                "" |] ;
    [|            "" ;                      "" ; "=(B4-(A4/(1.+1.)))" ;                "" |] ;
    [|            "" ;                      "" ; "=(B5-(A5/(1.+1.)))" ;                "" |] ;
    [|            "" ;                      "" ; "=(B6-(A6/(1.+1.)))" ;                "" |] ;
    [| "=SUM(A1:A6)" ; "=(SUM(B1:B6)/(1.+1.))" ; "=(B7-(A7/(1.+1.)))" ; "=AVERAGE(D1:D6)" |] ;
  |] in test_matrix expected result

let noisy_headings_test () =
  let result = run_on_values {
    constants = [] ;
    data = Matrix.Offsetted.create Value.[|
      [| String "HEAD"    ; String "Col 1" ; String "Col 2" ; String "Col 3" ; String "Col 4" |] ;
      [| String "Row 1"   ; Num 1.         ; Num 10.        ; Num 9.5        ; Num 24.        |] ;
      [| String "Row 2"   ; Num 23.        ; Num 12.        ; Num 0.5        ; Num 35.        |] ;
      [| String "Row 3"   ; Num 22.        ; Num 2.         ; Num (-9.)      ; Num 23.        |] ;
      [| String "Row 4"   ; Num (-1.)      ; Num 6.         ; Num 6.5        ; Num 5.         |] ;
      [| String "Row 5"   ; Num 59.        ; Num 0.         ; Num (-29.5)    ; Num 40.        |] ;
      [| String "Row 6"   ; Num 11.        ; Num (-2.)      ; Num (-7.5)     ; Num 9.         |] ;
      [| String "Row 7"   ; Num 0.         ; Num 44.        ; Num 44.        ; Num 18.        |] ;
      [| String "Row 9"   ; Num 115.       ; Num 36.        ; Num (-21.5)    ; Num 22.        |] ;
    |] ;
    mask = None ;
  }
  and expected = [|
    [| "" ;            "" ;                      "" ;                   "" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C2-(B2/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C3-(B3/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C4-(B4/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C5-(B5/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C6-(B6/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C7-(B7/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ;                      "" ; "=(C8-(B8/(1.+1.)))" ;                "" |] ;
    [| "" ; "=SUM(B1:B8)" ; "=(SUM(C1:C8)/(1.+1.))" ; "=(C9-(B9/(1.+1.)))" ; "=AVERAGE(E1:E8)" |] ;
  |] in test_matrix expected result

let range_bound_test () =
  let result = run_on_values {
    constants = [] ;
    data = Matrix.Offsetted.create ~top_left:(Some (1,1)) Value.[|
      [| String "HEAD"    ; String "Col 1" ; String "Col 2" ; String "Col 3" ; String "Col 4" |] ;
      [| String "Row 1"   ; Num 1.         ; Num 10.        ; Num 9.5        ; Num 24.        |] ;
      [| String "Row 2"   ; Num 23.        ; Num 12.        ; Num 0.5        ; Num 35.        |] ;
      [| String "Row 3"   ; Num 22.        ; Num 222.       ; Num 211.       ; Num 23.        |] ;
      [| String "Row 4"   ; Num (-1.)      ; Num 6.         ; Num 6.5        ; Num 5.         |] ;
      [| String "Row 5"   ; Num 59.        ; String "?"     ; String "???"   ; Num 40.        |] ;
      [| String "Row 6"   ; Num 11.        ; Num (-2.)      ; Num (-7.5)     ; Num 9.         |] ;
      [| String "Row 7"   ; Num 0.         ; Num 44.        ; Num 44.        ; Num 18.        |] ;
      [| String "Row 9"   ; Num 115.       ; Num 36.        ; Num (-21.5)    ; Num 22.        |] ;
    |] ;
    mask = None ;
  }
  and expected = [|
    [| "" ;            "" ; "" ;                   "" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C2-(B2/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C3-(B3/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C4-(B4/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C5-(B5/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ; "" ;                   "" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C7-(B7/(1.+1.)))" ;                "" |] ;
    [| "" ;            "" ; "" ; "=(C8-(B8/(1.+1.)))" ;                "" |] ;
    [| "" ; "=SUM(B2:B8)" ; "" ; "=(C9-(B9/(1.+1.)))" ; "=AVERAGE(E2:E8)" |] ;
  |] in test_matrix expected result

let large_constant_test () =
  let result = run_on_values {
    constants = [] ;
    data = Matrix.Offsetted.create Value.[|
      [| String "HEAD"    ; String "Col 1" ; String "Col 2" ; String "Col 3" ; String "Col 4" ; String "Col 5" |] ;
      [| String "Row 1"   ; Num 1.         ; Num 10.        ; Num 9.5        ; Num 24.        ; Num 9500.      |] ;
      [| String "Row 2"   ; Num 23.        ; Num 12.        ; Num 0.5        ; Num 35.        ; Num 500.       |] ;
      [| String "Row 3"   ; Num 22.        ; Num 222.       ; Num 211.       ; Num 23.        ; Num 211000.    |] ;
      [| String "Row 4"   ; Num (-1.)      ; Num 6.         ; Num 6.5        ; Num 5.         ; Num 6500.      |] ;
      [| String "Row 5"   ; Num 59.        ; String "?"     ; String "???"   ; Num 40.        ; String "???"   |] ;
      [| String "Row 6"   ; Num 11.        ; Num (-2.)      ; Num (-7.5)     ; Num 9.         ; Num (-7500.)   |] ;
      [| String "Row 7"   ; Num 0.         ; Num 44.        ; Num 44.        ; Num 18.        ; Num 44000.     |] ;
      [| String "Row 9"   ; Num 115.       ; Num 36.        ; Num (-21.5)    ; Num 22.        ; Num (-21500.)  |] ;
    |] ;
    mask = None ;
  }
  and expected = [|
    [| "" ;            "" ; "" ;                   "" ;                "" ;            "" |] ;
    [| "" ;            "" ; "" ; "=(C2-(B2/(1.+1.)))" ;                "" ; "=(D2*1000.)" |] ;
    [| "" ;            "" ; "" ; "=(C3-(B3/(1.+1.)))" ;                "" ; "=(D3*1000.)" |] ;
    [| "" ;            "" ; "" ; "=(C4-(B4/(1.+1.)))" ;                "" ; "=(D4*1000.)" |] ;
    [| "" ;            "" ; "" ; "=(C5-(B5/(1.+1.)))" ;                "" ; "=(D5*1000.)" |] ;
    [| "" ;            "" ; "" ;                   "" ;                "" ;            "" |] ;
    [| "" ;            "" ; "" ; "=(C7-(B7/(1.+1.)))" ;                "" ; "=(D7*1000.)" |] ;
    [| "" ;            "" ; "" ; "=(C8-(B8/(1.+1.)))" ;                "" ; "=(D8*1000.)" |] ;
    [| "" ; "=SUM(B1:B8)" ; "" ; "=(C9-(B9/(1.+1.)))" ; "=AVERAGE(E1:E8)" ; "=(D9*1000.)" |] ;
  |] in test_matrix expected result

let all = [
  "Point-wise column operations",         `Quick, pointwise_col_test ;
  "Point-wise row and column operations", `Quick, pointwise_row_col_test ;
  "Last row aggregation operations",      `Quick, last_row_aggregate_test ;
  "Noisy data: row and column headings",  `Quick, noisy_headings_test ;
  "Synthesis restricted to a range",      `Quick, range_bound_test ;
  "Inference of large constants",         `Quick, large_constant_test ;
]