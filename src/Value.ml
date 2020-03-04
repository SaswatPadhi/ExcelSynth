open Core_kernel

open Exceptions
open Utils

module T = struct
  type t = Bool of bool
         | Error
         | Merged of int * int
         | Num of float
         | String of string
         | Range of t list list
         [@@deriving compare,sexp]
end

include T

module Compare = Comparable.Make (T)

let equal v1 v2 = match v1 , v2 with
                  | Num f1, Num f2 -> Float.Approx.equal f1 f2
                  | _ -> Compare.equal v1 v2
[@@inline always]

let compare v1 v2 = match v1 , v2 with
                    | Num f1, Num f2 -> Float.Approx.compare f1 f2
                    | _ -> Compare.compare v1 v2
[@@inline always]

let rec typeof : t -> Type.t = Type.(function
                                     | Bool _   -> BOOL
                                     | Error    -> ERROR
                                     | Merged _ -> MERGED
                                     | Num _    -> NUM
                                     | String _ -> STRING
                                     | Range _  -> RANGE)

let majority_type (a : t array) : Type.t =
  let half_length_a = Float.of_int (Array.length a) /. 2. in
  let count = [| 0. ; 0. ; 0. ; 0. ; 0. ; 0. |]
   in Array.iter a ~f:Type.(fun v -> match typeof v with
                                     | BOOL   -> count.(0) <- count.(0) +. 1.
                                     | ERROR  -> count.(1) <- count.(1) +. 1.
                                     | MERGED -> count.(2) <- count.(2) +. 1.
                                     | NUM    -> count.(3) <- count.(3) +. 1.
                                     | STRING -> count.(4) <- count.(4) +. 1.
                                     | RANGE  -> count.(5) <- count.(5) +. 1.)
    ; Type.(match Array.findi count ~f:(fun _ c -> Float.(c >= half_length_a)) with
            | Some (0, _) -> BOOL
            | Some (3, _) -> NUM
            | Some (5, _) -> RANGE
            | _ -> raise NoMajorityType)

let rec to_string : t -> string = function
  | Bool b       -> String.uppercase (Bool.to_string b)
  | Error        -> "<error>"
  | Merged (x,y) -> "<m:" ^ (Excel.Cell.of_rc_ints x y) ^ ">"
  | Num n        -> Float.to_string n
  | String s     -> s
  | Range r
    -> "<range: "
     ^ (List.to_string_map r ~sep:","
                           ~f:(fun c -> "["
                                      ^ (List.to_string_map c ~sep:"," ~f:to_string)
                                      ^ "]"))
     ^ ">"

let of_string (s : string) : t =
  try
    Num (Float.of_string s)
  with Invalid_argument _ -> try
    let x , y = Excel.Cell.to_rc_ints (String.chop_prefix_exn ~prefix:"<m:"
                                                              (String.chop_suffix_exn ~suffix:">" s))
     in Merged (x,y)
  with Invalid_argument _ -> String.(
    if equal s "<error>" then Error
    else if equal s "TRUE" then Bool true
    else if equal s "FALSE" then Bool false
    else String s
  )