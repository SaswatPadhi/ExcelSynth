open Core

open Exceptions

module T = struct
  type t = NUM
         | BOOL
         | STRING
         | RANGE
         [@@deriving compare,sexp]
end

include T
include Comparable.Make (T)