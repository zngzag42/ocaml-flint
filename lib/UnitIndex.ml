open Core
module T =
  struct
    type t = unit [@@deriving compare, sexp]
  end
include T
include Comparable.Make(T)

let to_string () = "t"
let of_string s = ()
