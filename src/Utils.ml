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
  let rec to_col_name c =
    let q = c / 26
    in if q > 0 then (to_col_name (q - 1)) ^ Char.(to_string (of_int_exn ((c % 26) + 65)))
                else Char.(to_string (of_int_exn (c + 65)))

  let rec of_col_name col =
    (String.fold col ~init:0 ~f:(fun acc c -> 26*acc + (Char.to_int c) - 64)) - 1

  let to_cell_name r c = (to_col_name c) ^ (Int.to_string (r + 1))

  let of_cell_name cell =
    let col = String.take_while cell ~f:Char.is_alpha in
    let row = String.(drop_prefix cell (length col))
     in ((Int.of_string row)-1, of_col_name col)

  let offset_range range t l b r =
    let [@warning "-8"] [tl ; br] = String.split ~on:':' range in
    let tl = of_cell_name tl and br = of_cell_name br in
    let t = (fst tl) + t and l = (snd tl) + l
    and b = (fst br) + b and r = (snd br) + r
     in if t < 0 || l < 0 || b < t || r < l
        then raise (Invalid_argument "Bad offset!")
        else ((to_cell_name t l) ^ ":" ^ (to_cell_name b r))
end