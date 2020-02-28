open Core

open Exceptions

module List = struct
  include List

  let to_string_map ~(sep : string) (l : 'a list) ~(f : 'a -> string) : string =
    String.concat ~sep (List.map l ~f)
end

module Array = struct
  include Array

  let to_string_map ~(sep : string) (l : 'a array) ~(f : 'a -> string) : string =
    String.concat_array ~sep (map l ~f)
end

module Excel = struct
  module Column = struct
    let rec of_int c =
      let q = c / 26
      in if q > 0 then (of_int (q - 1)) ^ Char.(to_string (of_int_exn ((c % 26) + 65)))
                  else Char.(to_string (of_int_exn (c + 65)))

    let rec to_int col =
      (String.fold col ~init:0 ~f:(fun acc c -> 26*acc + (Char.to_int c) - 64)) - 1
  end

  module Cell = struct
    let of_rc_ints r c = (Column.of_int c) ^ (Int.to_string (r + 1))

    let to_rc_ints cell =
      let col = String.take_while cell ~f:Char.is_alpha in
      let row = String.(drop_prefix cell (length col))
      in ((Int.of_string row)-1, Column.to_int col)
  end

  module Range = struct
    let of_rc_ints r1 c1 r2 c2 =
      (Cell.of_rc_ints r1 c1) ^ ":" ^ (Cell.of_rc_ints r2 c2)

    let to_rc_ints r =
      let [@warning "-8"] [tl ; br] = String.split ~on:':' r
       in (Cell.to_rc_ints tl , Cell.to_rc_ints br)

    let offset range t l b r =
      let tl , br = to_rc_ints range in
      let t = (fst tl) + t and l = (snd tl) + l
      and b = (fst br) + b and r = (snd br) + r
      in if t < 0 || l < 0 || b < t || r < l
          then raise (Invalid_argument "Bad offset!")
          else of_rc_ints t l b r
  end
end