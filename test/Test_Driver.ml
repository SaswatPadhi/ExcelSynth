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
  let result = run ~config {
    constants = [] ;
    data = Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|   1. ;  10. ;   9.5 ;  11. |] ;
      [|  23. ;  12. ;   0.5 ;  35. |] ;
      [|  22. ;   2. ;   -9. ;  24. |] ;
      [|  -1. ;   6. ;   6.5 ;   5. |] ;
      [|  59. ;   0. ; -29.5 ;  59. |] ;
      [|  11. ;  -2. ;  -7.5 ;   9. |] ;
      [|  92. ;   0. ;  -46. ;  92. |] ;
    |] ;
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
  let result = run ~config {
    constants = [] ;
    data = Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|  1. ; 10. ;  0. ; 11. |] ;
      [| 23. ; 12. ;  7. ; 35. |] ;
      [| 22. ;  2. ;  7. ; 24. |] ;
      [| -1. ;  6. ; 23. ;  5. |] ;
      [| 59. ;  0. ;  3. ; 59. |] ;
      [| 11. ; -2. ; 33. ;  9. |] ;
      [| 92. ;  0. ; 43. ; 92. |] ;
    |] ;
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
  let result = run {
    constants = [] ;
    data = Array.(map ~f:(map ~f:(fun i -> Value.Num i))) [|
      [|   1. ;  10. ;   9.5 ;  24. |] ;
      [|  23. ;  12. ;   0.5 ;  35. |] ;
      [|  22. ;   2. ;   -9. ;  24. |] ;
      [|  -1. ;   6. ;   6.5 ;   5. |] ;
      [|  59. ;   0. ; -29.5 ;  41. |] ;
      [|  11. ;  -2. ;  -7.5 ;   9. |] ;
      [| 115. ;  14. ; -43.5 ;  23. |] ;
    |] ;
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

let all = [
  "Point-wise column operations",         `Quick, pointwise_col_test ;
  "Point-wise row and column operations", `Quick, pointwise_row_col_test ;
  "Last row aggregation operations",      `Quick, last_row_aggregate_test ;
]