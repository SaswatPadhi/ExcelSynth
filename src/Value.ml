open Core_kernel

open Exceptions
open Utils

module T = struct
  type t = Bool of bool
         | Error
         | Num of float
         | String of string
         | Range of t list list
         [@@deriving compare,sexp]
end

include T
include Comparable.Make (T)

let rec typeof : t -> Type.t = Type.(function
                                     | Bool _   -> BOOL
                                     | Error    -> ERROR
                                     | Num _    -> NUM
                                     | String _ -> STRING
                                     | Range _  -> RANGE)

let majority_type (a : t array) : Type.t =
  let half_length_a = Float.of_int (Array.length a) /. 2. in
  let count = [| 0. ; 0. ; 0. ; 0. ; 0. |]
   in Array.iter a ~f:Type.(fun v -> match typeof v with
                                     | BOOL   -> count.(0) <- count.(0) +. 1.
                                     | ERROR  -> count.(1) <- count.(1) +. 1.
                                     | NUM    -> count.(2) <- count.(2) +. 1.
                                     | STRING -> count.(3) <- count.(3) +. 1.
                                     | RANGE  -> count.(4) <- count.(4) +. 1.)
    ; Type.(match Array.findi count ~f:(fun _ c -> Float.(c >= half_length_a)) with
            | None -> raise NoMajorityType
            | Some (0, _) -> BOOL
            | Some (1, _) -> ERROR
            | Some (2, _) -> NUM
            | Some (3, _) -> STRING
            | Some (4, _) -> RANGE
            | _ -> raise (Internal_Exn "Impossible case"))

let rec to_string : t -> string = function
  | Bool b   -> Bool.to_string b
  | Error    -> "<error>"
  | Num n    -> Float.to_string n
  | String s -> s
  | Range r  -> "<"
              ^ (List.to_string_map r ~sep:","
                                    ~f:(fun c -> "["
                                               ^ (List.to_string_map c ~sep:"," ~f:to_string)
                                               ^ "]"))
              ^ ">"

let rec of_string (s : string) : t =
  try
    Num (Float.of_string s)
  with Invalid_argument _ -> try
    Bool (Bool.of_string s)
  with Invalid_argument _ ->
    if String.equal s "<error>" then Error else String s