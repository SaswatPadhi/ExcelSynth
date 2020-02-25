open Core

open Exceptions

module List = struct
  include List

  let to_string_map ~(sep : string) (l : 'a list) ~(f : 'a -> string) : string =
    String.concat ~sep (List.map l ~f)

  let to_string_mapi ~(sep : string) (l : 'a list) ~(f : int -> 'a -> string) : string =
    String.concat ~sep (mapi ~f l)

  let to_string_map2 ~(sep : string) (l1 : 'a list) (l2 : 'b list)
                     ~(f : 'a -> 'b -> string) : string =
    String.concat ~sep (map2_exn ~f l1 l2)

  let range_map ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive)
                ~(f : int -> 'a) (start_i : int) (stop_i : int) : 'a list =
    map ~f (range ~stride ~start ~stop start_i stop_i)
end

module Array = struct
  include Array

  let iteri2_exn (a1 : 'a array) (a2 : 'b array) ~(f : int -> 'a -> 'b -> unit) : unit =
    if length a1 <> length a2 then invalid_arg "Array.iter2_exn";
    iteri a1 ~f:(fun i x1 -> f i x1 a2.(i))

  let to_string_map ~(sep : string) (l : 'a array) ~(f : 'a -> string) : string =
    String.concat_array ~sep (map l ~f)

  let to_string_map2 ~(sep : string) (l1 : 'a array) (l2 : 'b array)
                     ~(f : 'a -> 'b -> string) : string =
    String.concat_array ~sep (map2_exn l1 l2 ~f)
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

let get_in_channel = function
  | "-"      -> Stdio.In_channel.stdin
  | filename -> Stdio.In_channel.create filename