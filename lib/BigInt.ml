module Fmpz = C.Function.Fmpz
open Core
open Fmpz

let create_ptr () =
  let out = Ctypes.allocate  ~finalise:clear C.Type.slong (Signed.Long.of_int 0) in
  let () = init out in
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


module T =
  struct

    type t= Fmpz.t
    let to_string x = get_str "" 10 x
    let of_string s =
       let out = create_ptr() in
         match set_str out s 10 with
         | 0 -> out
         | _ -> invalid_arg (s ^ "  isn't the string of a FlintInt")

    let sexp_of_t x = Sexp.Atom (to_string x)
    let t_of_sexp s =
      match s with
      | Sexp.Atom s -> of_string s
      | Sexp.List _ -> of_sexp_error "FlintInt.of_sexp: atom needed" s

    let compare x1 x2 = cmp x1 x2
  end

include T
let const_ptr x = x
let pp = (fun ppf x -> Format.fprintf ppf "%s" (to_string x))
include Comparable.Make(T)

let zero = create_from_fun zero

let one = create_from_fun one

let of_int i = create_from_fun (fun out -> set_si out (Signed.Long.of_int i))

let add x y = create_from_fun (fun out -> add out x y)

let neg x = create_from_fun (fun out -> neg out x)

let mult x y = create_from_fun (fun out -> mul out x y)

let inv x =
  if is_pm1 x
  then Some x
  else None

let inv_exn x =
  match inv x with
  | Some xinv -> xinv
  | None -> invalid_arg (to_string x ^ " doesn't have an inverse")

let sub x y = create_from_fun (fun out -> sub out x y)

let pow x n =
  let out = create_ptr() in
  match pow out x (of_int n) with
  | 1 -> out
  | _ -> invalid_arg "pow failed"

let div_rem x y =
  let q = create_ptr() in
  let r = create_ptr() in
  let () = tdiv_qr q r x y in
  (q,r)

let gcd x y = create_from_fun (fun out -> gcd out x y)

let lcm x y = create_from_fun (fun out -> lcm out x y)

let standard_unit x  =
  if x >= zero
  then (one, x)
  else (neg one, neg x)
