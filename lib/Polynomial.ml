module type S =
  sig
    type index

    type t

    include RAlg.S with type t := t and type Index.t = (index * int) list

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val pp : Format.formatter -> t -> unit

    val of_string : string -> t

    val leading : t -> R.t
    val leading_term : t -> R.t * ((index * int) list)

    val const : R.t -> t
    val of_mono : R.t -> (index * int) list -> t
    (*val of_mlist : (R.t * (index * int) list) list -> t *)

    val degree : t -> int
    (* Maximum degree of variable i *)
    val idegree : t -> i:index -> int

    (* Number of terms in p *)
    val length : t -> int
    (* eval p f == p(f_1,...,f_n) *)
    val eval : t -> (index -> R.t) -> R.t

    (* Computes the polynomial found by subsituting f(i) for the variable i *)
    val subs : t -> f:(index -> t) -> t

    (* WARNING: For multivariable polynomials remainders are not well defined unless we are using a Groebner basis*)
    val div_rem : t -> t -> (t * t)

    (* Computes the gcd of the two polynomials using order inherited from Index *)
    val gcd : t -> t -> t
    val lcm : t -> t -> t
    (* Returns standard_unit r = (u,s) where u*r =s
     * u is a constant and the leading term of s is "standardized" *)
    val standard_unit : t -> (t*t)

    val derivative : t -> (t * index) list

end
