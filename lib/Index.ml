open Core
module type S =
  sig
    type t
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t

    include Comparable with type t:= t
    val to_string : t -> string
    val of_string : string -> t
  end
