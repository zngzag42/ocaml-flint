open Core
open Ctypes

module Fmpz_mpoly = C.Function.Fmpz_mpoly
module Fmpz = C.Function.Fmpz
let slong = C.Type.slong

module Ord =
  struct
    include Fmpz_mpoly.Ordering
  end

module type CTX =
  sig
    val vars : string list
    val ord : Ord.t
  end

module Make(Ctx: CTX) :
sig
  include Polynomial.S with type index = string and module R = BigInt
  val create_from_fun : (Fmpz_mpoly.t -> unit) -> t
  val create_from_funB : ?error:string -> (Fmpz_mpoly.t -> bool) -> t
  val create_from_funErr : ?error:string -> (Fmpz_mpoly.t -> int) -> t
  val const_ptr : t -> Fmpz_mpoly.t
end =
  struct
    let ctx =
      let out = allocate ~finalise:Fmpz_mpoly.Ctx.clear Fmpz_mpoly.Ctx.s (make Fmpz_mpoly.Ctx.s) in
      let () = Fmpz_mpoly.Ctx.init out (Signed.Long.of_int (List.length Ctx.vars)) Ctx.ord in
      out
    let nvars = List.length Ctx.vars
    let varindex = String.Map.of_alist_exn (List.mapi Ctx.vars ~f:(fun i s -> (s,i)))
    let varname = Array.of_list Ctx.vars
    let var_to_i s =
      match String.Map.find varindex s with
      | Some i -> i
      | None -> invalid_arg (s ^ " isn't in the context of current polynomial")

    type index = string

    module Index =
      struct
        module T =
          struct
            type t = (string * int) list  [@@deriving compare, sexp]
          end
        include T
        include Comparable.Make(T)
        let simplify l =
          l |> String.Map.of_alist_reduce ~f:(+) |> String.Map.to_alist |> List.sort ~compare:(fun (s1,_) (s2,_) -> Int.compare (var_to_i s1) (var_to_i s2))
        let to_uis l =
          let powmap = l |> String.Map.of_alist_reduce ~f:(+) in
          let pow v = match String.Map.find powmap v with Some d -> d | None -> 0 in
          Ctx.vars |> List.map ~f:pow |> List.map ~f:Unsigned.ULong.of_int
        let of_ints l =
          List.filter_mapi l ~f:(fun i d ->  if Int.(d=0) then None else Some(Array.get varname i, d))

        let to_string l =
          l |> simplify
          |> List.map ~f:(fun (v,i) -> v ^ "^" ^Int.to_string i )
          |> String.concat ~sep:"*"

        let of_string (s:string) : t =
          List.filter_map (String.split s ~on:'*')
            ~f:(fun v -> match String.split v ~on:'^' with
                         | [] -> invalid_arg (v ^" is not a v^d")
                         | ["1"] -> None
                         | [x] -> if String.Map.mem varindex x
                                  then Some(x, 1)
                                  else invalid_arg (x ^ " isn't a variable in this context: " ^ String.concat ~sep:" "Ctx.vars )
                         | ["1";d] -> None
                         | [x;d] -> if String.Map.mem varindex x
                                    then Some(x, Int.of_string d)
                                    else invalid_arg (x ^ " isn't a variable in this context: " ^ String.concat ~sep:" "Ctx.vars)
                         | _ -> invalid_arg (v ^ " is not of form v^d"))
          |> simplify
      end

    module R = BigInt

    let create_ptr () =
      let out = Ctypes.allocate ~finalise:(fun ptr -> Fmpz_mpoly.clear ptr ctx) Fmpz_mpoly.s (Ctypes.make Fmpz_mpoly.s) in
      let () = Fmpz_mpoly.init out ctx in
      out

    let create_from_fun f =
      let out = create_ptr() in
      let () = f out in
      out

    let create_from_funB ?error:(error="fmpz_mpoly operation failed") f =
      let out = create_ptr() in
      if f out
      then out
      else invalid_arg error

    let create_from_funErr ?error:(error="fmpz_mpoly operation failed") f =
      let out = create_ptr() in
      if Int.(f out = 0)
      then out
      else invalid_arg error

    let const_ptr p = p

    module T =
      struct
        type t = Fmpz_mpoly.t

        let at_list p l =
          let pow_array = CArray.of_list ulong l in
          R.create_from_fun (fun out -> Fmpz_mpoly.get_coeff_fmpz_ui out p (CArray.start pow_array) ctx)

        let at p ~i = at_list p (Index.to_uis i)

        let degrees p =
          let max_degs = CArray.make slong nvars in
          let () = Fmpz_mpoly.degrees_si (CArray.start max_degs) p ctx in
          max_degs |> CArray.to_list
          |> List.map ~f:(fun d -> Signed.Long.to_int d)
          |> List.map ~f:(fun d -> if Int.(d < 0) then 0 else d)

        let rec all_less l =
          match l with
          | [] -> [[]]
          | x::xs -> xs |> all_less |> List.concat_map ~f:(fun acc -> List.init (x+1) ~f:(fun i -> i::acc))

        let to_list p =
          let indexes = p |> degrees |> all_less  in
          List.filter_map indexes ~f:(fun i -> let c = at_list p (List.map ~f:Unsigned.ULong.of_int i) in
                                     if R.(c = zero)
                                     then None
                                     else Some(c, List.mapi ~f:(fun v d -> (Array.get varname v, d)) i))


        let of_list (l:( R.t*Index.t) list) =
          let p = create_ptr() in
          let () = List.iter l ~f:(fun (c,d) ->
                       let uiarray = CArray.of_list ulong (Index.to_uis d) in
                       Fmpz_mpoly.set_coeff_fmpz_ui p (R.const_ptr c) (CArray.start uiarray) ctx) in
          p


        let sexp_of_t p = List.sexp_of_t (Tuple2.sexp_of_t R.sexp_of_t Index.sexp_of_t) (to_list p)
        let t_of_sexp s =
          s |>  List.t_of_sexp (Tuple2.t_of_sexp R.t_of_sexp Index.t_of_sexp)
          |> of_list

        let compare p1 p2 = Fmpz_mpoly.cmp p1 p2 ctx
      end

    include T

    let degree p = p |> degrees |> List.fold ~f:(+) ~init:0
    let idegree p ~i = Fmpz_mpoly.degree_si p (Signed.Long.of_int (var_to_i i)) ctx |> Signed.Long.to_int
    let length p = Fmpz_mpoly.length p ctx |> Signed.Long.to_int


    let to_string p =
      let names = CArray.of_list string Ctx.vars in
      Fmpz_mpoly.get_str_pretty p (CArray.start names) ctx

    let pp ppf poly = Format.fprintf ppf "%s" (to_string poly)

    let zero = create_from_fun (fun out -> Fmpz_mpoly.zero out ctx)
    let one = create_from_fun (fun out -> Fmpz_mpoly.one out ctx)
    let const c = create_from_fun (fun out -> Fmpz_mpoly.set_fmpz out (R.const_ptr c) ctx)


    let of_string s =
      let names = CArray.of_list string Ctx.vars in
      create_from_funErr ~error:(s ^ " isn't a valid polynomial string") (fun out -> Fmpz_mpoly.set_str_pretty out s (CArray.start names) ctx)

    let of_mono c l = of_list [c, l]

    let add p1 p2 = create_from_fun (fun res -> Fmpz_mpoly.add res p1 p2 ctx)
    let sub p1 p2 = create_from_fun (fun res -> Fmpz_mpoly.sub res p1 p2 ctx)
    let neg p = create_from_fun (fun res -> Fmpz_mpoly.neg res p ctx)
    let scale c p = create_from_fun (fun res -> Fmpz_mpoly.scalar_mul_fmpz res p (R.const_ptr c) ctx)
    let mult p1 p2 = create_from_fun (fun res -> Fmpz_mpoly.mul res p1 p2 ctx)

    let inv p =
      if Fmpz_mpoly.is_fmpz p ctx
      then if R.(equal (create_from_fun (fun res -> Fmpz_mpoly.get_fmpz res p ctx)) one)
              || R.(equal (create_from_fun (fun res -> Fmpz_mpoly.get_fmpz res p ctx)) (neg one))
           then Some p
           else None
      else None

    let inv_exn p =
      match inv p with
      | Some pinv -> pinv
      | None -> invalid_arg (to_string p ^ " doesn't have an inverse")

    let pow p n = create_from_funB (fun res -> Fmpz_mpoly.pow_ui res p (Unsigned.ULong.of_int n) ctx)

    let leading p = at_list p (List.map ~f:Unsigned.ULong.of_int (degrees p))
    let leading_term p = (leading p, Index.of_ints(degrees p))

    let terms p = p |> to_list |> List.filter_map ~f:(fun (r,i) -> if R.(r = zero) then None else Some i)
    let all_terms ls = List.dedup_and_sort ~compare:Index.compare (List.concat_map ls ~f:terms)
    let to_mat ls =
      let terms = all_terms ls in
      List.transpose_exn (List.map ls ~f:(fun l -> List.map terms ~f:(fun i -> at l ~i:i)))

    let to_sparse_mat ls =
      let index_position = Index.Map.of_alist_exn (List.mapi (all_terms ls) ~f:(fun i x -> (x,i))) in
      List.concat_mapi ls
        ~f:(fun j l -> l |> to_list |> List.map ~f:(fun (r,i) -> (Index.Map.find_exn index_position i, j ,r)))


    let map_basis ~f p =
      p |> to_list
      |> List.map ~f:(fun (c,i) -> (c, f i))
      |> of_list

    let mapi ~f p =
      p |> to_list
      |> List.map ~f:(fun (c,i) -> (f i c, i))
      |> of_list

    let filteri ~f p =
      p |> to_list
      |> List.filter ~f:(fun (c,i) -> f i c)
      |> of_list

    let eval p f =
      let cs = CArray.of_list Fmpz.t (List.map ~f:(fun i -> i |> f |> R.const_ptr) Ctx.vars) in
      R.create_from_funB ~error:"Can't evaluate poly" (fun res -> Fmpz_mpoly.evaluate_all_fmpz res p (CArray.start cs) ctx)

    let derivative p = List.map Ctx.vars ~f:(fun v -> (create_from_fun (fun out -> Fmpz_mpoly.derivative out p (Signed.Long.of_int (var_to_i v)) ctx),v))

    let subs p ~f =
      let fs = CArray.of_list Fmpz_mpoly.t (List.map ~f:f Ctx.vars) in
      create_from_funB ~error:"Can't subsitute polys" (fun res -> Fmpz_mpoly.compose_fmpz_mpoly res p (CArray.start fs) ctx ctx)

    let div_rem p d =
      let q = create_ptr() in
      let r = create_ptr() in
      let () = Fmpz_mpoly.divrem q r p d ctx in
      (q,r)

    let gcd p1 p2 = create_from_funB ~error:"Can't take gcd" (fun res -> Fmpz_mpoly.gcd res p1 p2 ctx)
    let lcm p1 p2 = create_from_fun (fun res -> Fmpz_mpoly.div res (mult p1 p2) (gcd p1 p2) ctx)

    let standard_unit p =
      match Ordering.of_int( R.compare (leading p) R.zero) with
      | Greater -> (one, p)
      | Equal -> (one, p)
      | Less -> (neg one, neg p)


    include Comparable.Make(T)

  end
