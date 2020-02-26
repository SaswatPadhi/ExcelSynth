open Core_kernel

open Exceptions
open Utils

module T = struct
  type t = Num of float
         | Bool of bool
         | String of string
         | Range of t list list
         [@@deriving compare,sexp]
end

include T
include Comparable.Make (T)

let rec typeof : t -> Type.t = function
  | Num _    -> Type.NUM
  | Bool _   -> Type.BOOL
  | String _ -> Type.STRING
  | Range _  -> Type.RANGE

let rec to_string : t -> string = function
  | Num n    -> Float.to_string n
  | Bool b   -> Bool.to_string b
  | String s -> s
  | Range r  -> raise (Internal_Exn "Ranges should not be (de/)serialized!")

let rec of_string (s : string) : t =
  try
    Num (Float.of_string s)
  with Invalid_argument _ -> try
    Bool (Bool.of_string s)
  with Invalid_argument _ ->
    String s