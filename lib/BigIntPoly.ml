open Core
module Fmpz_poly = C.Function.Fmpz_poly

type index = unit

module Index =
  struct
    module T =
      struct
        type t = (unit * int) list  [@@deriving compare, sexp]
        let total_deg l = List.fold ~init:0 ~f:(fun acc ((),d) -> acc+d) l
        let compare m1 m2 = Int.compare (total_deg m1) (total_deg m2)
      end
    include T
    include Comparable.Make(T)
    let to_string l = "t^"^(Int.to_string (total_deg l))
    let of_string (s:string) : t =
      List.filter_map (String.split s ~on:'*') ~f:(fun v -> match String.split v ~on:'^' with
                                                            | [] -> invalid_arg (v ^" is not a v^d")
                                                            | ["1"] -> None
                                                            | [x] -> Some((), 1)
                                                            | ["1";d] -> None
                                                            | [x;d] -> Some((), Int.of_string d)
                                                            | _ -> invalid_arg (v ^ " is not of form v^d"))
      |> total_deg
      |> (fun d -> [(),d])
  end

module R = BigInt

let create_ptr () =
  let out = Ctypes.allocate ~finalise:Fmpz_poly.clear Fmpz_poly.s (Ctypes.make Fmpz_poly.s) in
  let () = Fmpz_poly.init out in
  out

let create_from_fun f =
  let out = create_ptr() in
  let () = f out in
  out
let const_ptr p = p

module T =
  struct
    type t = Fmpz_poly.t
    let at p ~i = R.create_from_fun (fun out -> Fmpz_poly.get_coeff_fmpz out  p (Signed.Long.of_int (Index.total_deg i)))
    let to_list p =
      List.init (p |> Fmpz_poly.length |> Signed.Long.to_int) ~f:(fun i -> (at p ~i:[(),i], [(),i]))
    let to_alist p =
      List.init (p |> Fmpz_poly.length |> Signed.Long.to_int) ~f:(fun i -> at p ~i:[(),i])

    let of_list (l:( R.t*Index.t) list) =
      let p = Ctypes.allocate Fmpz_poly.s (Ctypes.make Fmpz_poly.s) in
      let degree = match l |> List.map ~f:snd |> List.max_elt ~compare:Index.compare with None -> 0 | Some d -> Index.total_deg d in
      let () = Fmpz_poly.init2 p (Signed.Long.of_int degree) in
      let () = List.iter l ~f:(fun (c,d) -> Fmpz_poly.set_coeff_fmpz p (Signed.Long.of_int (Index.total_deg d)) (R.const_ptr c)) in
      p

    let of_alist l = l |> List.mapi ~f:(fun i x -> (x,[(),i])) |> of_list

    let sexp_of_t p = List.sexp_of_t R.sexp_of_t (to_alist p)
    let t_of_sexp s =
      s |>  List.t_of_sexp R.t_of_sexp
      |> of_alist
    let degree p =
      match Signed.Long.to_int (Fmpz_poly.degree p) with
      | -1 -> 0
      | d -> d

    let compare p1 p2 =
      let rec loop p1 p2 n =
        if n < 0
        then Equal
        else (match Ordering.of_int (Signed.Long.compare (Fmpz_poly.get_coeff_si p1 (Signed.Long.of_int n))
                                       (Fmpz_poly.get_coeff_si p2 (Signed.Long.of_int n))) with
              | Equal -> loop p1 p2 (n-1)
              | cmp-> cmp) in
      (match Ordering.of_int (Int.compare (degree p1) (degree p2)) with
       | Less -> Less
       | Greater -> Greater
       | Equal -> loop p1 p2 (degree p1))
      |> Ordering.to_int
  end
include T


let idegree p ~i:() = degree p
let length p = Signed.Long.to_int (Fmpz_poly.length p)


let to_string p = Fmpz_poly.get_str_pretty p "t"
let pp ppf poly = Format.fprintf ppf "%s" (Fmpz_poly.get_str_pretty poly "t")



let zero = of_alist [R.zero]
let one = of_alist [R.one]
let const c = of_alist [c]


let of_string s =
  let res = Ctypes.allocate Fmpz_poly.s (Ctypes.make Fmpz_poly.s) in
  match Fmpz_poly.set_str res s with
  | 0 -> res
  | _ -> invalid_arg ("Can't parse " ^ s ^ " as a flint polynomial. Expected form  <length>  <c1> <c2> ... <clength>")

let of_mono c l = of_list [c, l]

let add p1 p2 = create_from_fun (fun res -> Fmpz_poly.add res p1 p2)
let sub p1 p2 = create_from_fun (fun res -> Fmpz_poly.sub res p1 p2)
let neg p = create_from_fun (fun res -> Fmpz_poly.neg res p)
let scale c p = create_from_fun (fun res -> Fmpz_poly.scalar_mul_fmpz res p (R.const_ptr c))
let mult p1 p2 = create_from_fun (fun res -> Fmpz_poly.mul res p1 p2)

let inv p =
  if Fmpz_poly.is_unit p
  then Some p (* Only unit polynomials are 1,-1 which are there own inverse *)
  else None

let inv_exn p =
  match inv p with
  | Some pinv -> pinv
  | None -> invalid_arg (to_string p ^ " doesn't have an inverse")

let pow p n = create_from_fun (fun res -> Fmpz_poly.pow res p (Unsigned.ULong.of_int n))

let leading p = at p ~i:[(),degree p]
let leading_term p = (leading p, [(),degree p])

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
  let c = R.const_ptr (f()) in
  R.create_from_fun (fun res -> Fmpz_poly.evaluate_fmpz res p c)

let derivative p = [create_from_fun (fun out -> Fmpz_poly.derivative out p),()]

let subs p ~f = invalid_arg "NYI"

let div_rem p d =
  let q = Ctypes.allocate Fmpz_poly.s (Ctypes.make Fmpz_poly.s) in
  let r = Ctypes.allocate Fmpz_poly.s (Ctypes.make Fmpz_poly.s) in
  let () = Fmpz_poly.div_rem q r p d in
  (q,r)

let gcd p1 p2 = create_from_fun (fun res -> Fmpz_poly.gcd res p1 p2)
let lcm p1 p2 = create_from_fun (fun res -> Fmpz_poly.lcm res p1 p2)

let standard_unit p =
  match Ordering.of_int( R.compare (leading p) R.zero) with
  | Greater -> (one, p)
  | Equal -> (one, p)
  | Less -> (neg one, neg p)


include Comparable.Make(T)
