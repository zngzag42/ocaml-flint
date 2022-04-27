(* module Fmpz_poly = FlintPolynmoial.Fmpz_poly*)
module Fmpz_poly = C.Function.Fmpz_poly
module Fmpz_poly_mat = C.Function.Fmpz_poly_mat
open Core
open Ctypes
let create_ptr (nrows, ncols) =
  let out = allocate ~finalise:Fmpz_poly_mat.clear Fmpz_poly_mat.s (make Fmpz_poly_mat.s) in
  let () = Fmpz_poly_mat.init out (Signed.Long.of_int nrows) (Signed.Long.of_int ncols) in
  out

let create_from_fun (nrows,ncols) f =
  let out = create_ptr(nrows, ncols) in
  let () = f out in
  out


module R = BigIntPoly
module Index = MatrixIndex


module T =
  struct
    include Fmpz_poly_mat
    let at mat ~i:(r,c) = R.create_from_fun (fun p -> Fmpz_poly.set p (entry mat (Signed.Long.of_int r) (Signed.Long.of_int c)))

    let rows mat = List.init (Signed.Long.to_int (nrows mat)) ~f:(fun r -> List.init (Signed.Long.to_int (ncols mat)) ~f:(fun c -> at mat ~i:(r,c) ))
    let of_rows rows =
      let mat = allocate s (make s) in
      let nrows = List.length rows in
      let ncols = match List.dedup_and_sort (List.map ~f:List.length rows) ~compare:Int.compare with
        | [ncols] -> ncols
        | _ -> invalid_arg "all rows must have the same number of entries" in
      let () = init mat (Signed.Long.of_int nrows) (Signed.Long.of_int ncols) in
      let () = List.iteri rows ~f:(fun r row -> List.iteri row ~f:(fun c ent ->
                                                    Fmpz_poly.set (entry mat (Signed.Long.of_int r) (Signed.Long.of_int c)) (R.const_ptr ent))) in
      mat

    let sexp_of_t mat = mat |> rows |> List.sexp_of_t (List.sexp_of_t R.sexp_of_t)
    let t_of_sexp sexp = sexp |> List.t_of_sexp (List.t_of_sexp R.t_of_sexp) |> of_rows
    let compare mat1 mat2 = List.compare (List.compare R.compare) (rows mat1) (rows mat2)
  end
include T

let dim mat = (Signed.Long.to_int (nrows mat), Signed.Long.to_int(ncols mat))

let row mat r = List.init (Signed.Long.to_int (ncols mat)) ~f:(fun c -> at mat ~i:(r,c))

let col mat c = List.init (Signed.Long.to_int (nrows mat)) ~f:(fun r -> at mat ~i:(r,c))
let cols mat = List.init (Signed.Long.to_int (ncols mat)) ~f:(fun c -> List.init (Signed.Long.to_int (nrows mat)) ~f:(fun r -> at mat ~i:(r,c)))

let to_string mat =
  mat |> rows
  |> List.map ~f:(List.map ~f:R.to_string)
  |> List.map ~f:(String.concat ~sep:",")
  |> String.concat ~sep:",\n"
  |> (fun s -> "{" ^ s ^ "}")

let id = to_string
(* TODO: Replace with pp that properly blocks matrix elements *)
let pp = (fun ppf x -> Format.fprintf ppf "%s" (to_string x))



let one n = create_from_fun (n,n) one

let zero n = create_from_fun (n,n) zero

let of_cols cols =
  let mat = allocate s (make s) in
  let ncols = List.length cols in
  let nrows = match List.dedup_and_sort (List.map ~f:List.length cols) ~compare:Int.compare with
    | [nrows] -> nrows
    | _ -> invalid_arg "all rows must have the same number of entries" in
  let () = init mat (Signed.Long.of_int nrows) (Signed.Long.of_int ncols) in
  let () = List.iteri cols ~f:(fun c col -> List.iteri col ~f:(fun r ent -> Fmpz_poly.set (entry mat (Signed.Long.of_int r) (Signed.Long.of_int c)) ent)) in
  mat

let transpose mat = create_from_fun (Tuple2.swap (dim mat)) (fun newmat -> transpose newmat mat)

let trace mat = R.create_from_fun (fun p -> trace p mat)

let det mat = R.create_from_fun (fun p -> det p mat)

let inv mat =
  let det = allocate Fmpz_poly.s (make Fmpz_poly.s) in
  let () = Fmpz_poly.init det in
  let out = allocate s (make s) in
  let () = init out (nrows mat) (ncols mat) in
  match inv out det mat with
  | 1 -> Some (out)
  | _ -> None

let add mat1 mat2 = create_from_fun (dim mat1) (fun out -> add out mat1 mat2)

let sub mat1 mat2 = create_from_fun (dim mat1) (fun out -> sub out mat1 mat2)

let neg mat = create_from_fun (dim mat) (fun out -> neg out mat)

let mult mat1 mat2 = create_from_fun (dim mat1) (fun out -> mul out mat1 mat2)

let pow mat n =
  if n >= 0
  then create_from_fun (dim mat) (fun out -> pow out mat (Unsigned.ULong.of_int n) )
  else invalid_arg "Can only take positive powers"

let init (m,n) ~f =
  let mat = allocate s (make s) in
  let () = init mat (Signed.Long.of_int m) (Signed.Long.of_int n) in
  let () = List.iter (List.range 0 m)
             ~f:(fun r -> List.iter (List.range 0 n)
                            ~f:(fun c -> Fmpz_poly.set (entry mat (Signed.Long.of_int r) (Signed.Long.of_int c)) (R.const_ptr (f r c)))) in
  mat

let to_list m =
  m
  |> rows
  |> List.concat_mapi ~f:(fun i -> List.mapi ~f:(fun j x -> (x,(i,j))))
  |> List.filter ~f:(fun (x,_) -> not(R.equal x R.zero))

let of_list l =
  let lookup = l |> List.map ~f:(fun (x,y) -> (y,x)) |> Index.Map.of_alist_exn in
  let m =
    match l |> List.map ~f:(fun (_,(i,_)) -> i) |> List.max_elt ~compare:Int.compare with
    | Some m -> m+1
    | None -> 0 in
  let n =
    match l |> List.map ~f:(fun (_,(_,j)) -> j) |> List.max_elt ~compare:Int.compare with
    | Some n -> n+1
    | None -> 0
  in
  init (m,n) ~f:(fun i j -> match Index.Map.find lookup (i,j) with Some x -> x | None -> R.zero)

let terms l = l |> to_list |> List.map ~f:snd
let all_terms l = l |> List.concat_map ~f:terms|> List.dedup_and_sort ~compare:Index.compare
let to_mat ls =
  let terms = all_terms ls in
  List.transpose_exn (List.map ls ~f:(fun l -> List.map terms ~f:(fun i -> at l ~i:i)))

let to_sparse_mat ls =
  let index_position = Index.Map.of_alist_exn (List.mapi (all_terms ls) ~f:(fun i x -> (x,i))) in
  List.concat_mapi ls ~f:(fun j l -> List.map (terms l) ~f:(fun index -> (Index.Map.find_exn index_position index, j ,at l ~i:index)))

let mapi ~f m = init (dim m) ~f:(fun i j -> f (i,j) (at ~i:(i,j) m))
let map_basis ~f m = init (dim m) ~f:(fun i j -> at ~i:(f (i,j)) m)
let filteri ~f m = init (dim m) ~f:(fun i j -> if f (i,j) (at ~i:(i,j) m) then at ~i:(i,j) m  else R.zero)


include Comparable.Make(T)
