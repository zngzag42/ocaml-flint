module type S =
  sig
    type t
    (* include Index.S with type t := t*)
    include RAlg.S with type t := t and type Index.t := int*int

    val id : t -> string

    val dim : t -> int * int

    val row : t -> int -> R.t list
    val col : t -> int -> R.t list

    val rows : t -> R.t list list
    val cols : t -> R.t list list

    val init : int * int -> f:(int -> int -> R.t) -> t

    (* one n is the nxn identity matrix *)
    val one : int -> t
    (* zero n is the nxn matrix of all zeros *)
    val zero : int -> t

    val of_rows : R.t list list -> t
    val of_cols : R.t list list -> t

    val transpose : t -> t
    val trace : t -> R.t
    val det : t-> R.t

  end
