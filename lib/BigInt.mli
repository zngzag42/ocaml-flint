include Ring.EuclideanDomain
val create_from_fun : (C.Function.Fmpz.t -> unit) -> t
val create_from_funB : ?error:string -> (C.Function.Fmpz.t -> bool) -> t
val const_ptr : t -> C.Function.Fmpz.t
val of_int : int -> t
