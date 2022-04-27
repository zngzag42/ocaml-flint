open Core
module T =
  struct
    type t = int*int [@@deriving compare, sexp]
  end
include T
include Comparable.Make(T)

let to_string (i,j) = "e" ^ Int.to_string i ^"_" ^ Int.to_string j
let of_string s =
  match String.split (String.drop_prefix s 1) ~on:'_' with
  | [i;j] -> (Int.of_string i, Int.of_string j)
  | _ -> invalid_arg (s ^ " is not ei_j")
