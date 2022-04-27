module type S =
  sig
    module R : Ring.S
    module Index : Index.S
    type t
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val compare : t -> t -> int
    val equal : t -> t -> bool

    val to_string : t -> string
    val to_list : t -> (R.t * Index.t) list
    val terms : t -> Index.t list

    val all_terms : t list -> Index.t list
    val to_mat : t list -> R.t list list
    val to_sparse_mat : t list -> (int*int*R.t) list


    val zero : t
    val of_list : (R.t * Index.t) list  -> t
    val add : t -> t -> t
    val scale : R.t -> t -> t
    val at : t -> i:Index.t -> R.t

    val map_basis : f:(Index.t-> Index.t) -> t -> t
    val mapi : f:(Index.t -> R.t -> R.t) -> t -> t
    val filteri : f:(Index.t -> R.t -> bool) -> t -> t

    val sub : t -> t -> t
  end
