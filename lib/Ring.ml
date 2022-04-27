open Core

module type S =
  sig
    type t
    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t

    include Comparable.S with type t := t
    val to_string : t -> string
    val of_string : string -> t
    val pp : Format.formatter -> t -> unit

    val zero : t
    val one : t
    val add : t -> t -> t
    val neg : t -> t
    val mult : t -> t -> t
    val inv : t -> t Option.t
    val inv_exn : t -> t

    val sub : t -> t -> t
    val pow : t -> int -> t
  end

module type EuclideanDomain (* TODO: Seperate Euclidean domain from UFD *) =
  sig
    include S

    (* (q,r) = div_rem p d iff p = q*d + r where r <= d *)
    val div_rem : t -> t -> t * t

    (* gcd x y = g iff div_rem x g = (_,0) and div_rem y g = (_,0) and no "larger g" has this property *)
    val gcd : t -> t -> t

    (* lcm x y = g iff div_rem g x = (_,0) and div_rem g y = (_,0) and no "smaller g" has this property *)
    val lcm : t -> t -> t

    (* standard_unit r = (u,s) where u*r = s and u is a unit such that s is "standard"
     * i.e for integers s is positive, for polynomials s has leading coefficient of 1 *)
    val standard_unit : t -> (t * t)
  end
