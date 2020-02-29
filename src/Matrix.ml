open Core

type 'a t = 'a array array

module Offsetted = struct
  module T = struct
    type nonrec 'a t = Offsetted of 'a t * (int * int) * (int * int) * 'a t
  end

  let create ?(top_left = None) ?(bottom_right = None) (mat : 'a t) : 'a T.t =
    let r1,c1 = match top_left with None -> 0,0
                    | Some (r1,c1) -> (r1,c1)
    and r2,c2 = match bottom_right with None -> Array.((length mat) - 1, (length mat.(0)) - 1)
                    | Some (r2,c2) -> (r2,c2)
     in
    let submat = Array.(filter_mapi mat ~f:(fun i row -> if not (r1 <= i && i <= r2) then None
                                                         else Some (filteri row ~f:(fun j _ -> c1 <= j && j <= c2))))
     in Offsetted (mat , (r1,c1) , (r2,c2) , submat)

  let merge ((Offsetted (mat , (r1,c1) , _ , submat)) : 'a T.t) : 'a t =
    Array.(iteri submat ~f:(fun i row -> iteri row ~f:(fun j cell -> mat.(i+r1).(j+c1) <- cell))) ; mat
  [@@inline always]

  let submatrix ((Offsetted (_ , _ , _ , submat)) : 'a T.t) : 'a t = submat
  [@@inline always]

  let top_left ((Offsetted (_ , tl , _ , _)) : 'a T.t) : int * int = tl

  let bottom_right ((Offsetted (_ , _ , br , _)) : 'a T.t) : int * int = br

  include T
end

module Infix = struct
  let ( !> ) (m : 'a Offsetted.t) = Offsetted.submatrix m [@@inline always]
  let ( !! ) ((Offsetted (mat , _ , _, _)) : 'a Offsetted.t) = mat [@@inline always]
end