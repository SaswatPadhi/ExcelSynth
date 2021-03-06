open Core

open Exceptions

module T = struct
  type t = BOOL
         | ERROR
         | MERGED
         | NUM
         | STRING
         | RANGE
         [@@deriving compare,sexp]
end

include T
include Comparable.Make (T)