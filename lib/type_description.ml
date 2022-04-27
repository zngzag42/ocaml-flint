open Ctypes

module Types(F: TYPE) =
  struct
    open F

    (* TOOD: Figure out to check what is #defined *)
    type mp_limb_t = Unsigned.ulong
    let mp_limb_t = typedef ulong "mp_limb_t"
    type mp_limb_signed_t = Signed.long
    let mp_limb_signed_t = typedef long "mp_limb_signed_t"

    type ulong = mp_limb_t
    let ulong = mp_limb_t



    type slong = mp_limb_signed_t
    let slong = mp_limb_signed_t


    type fmpz = slong
    let fmpz = typedef slong "fmpz"
    module Fmpz =
      struct
        type t = fmpz ptr
        let t = ptr fmpz
      end
    module Fmpz_mat =
      struct
        type s
        let s : s Ctypes.structure typ = typedef (F.structure "fmpz_mat_struct") "fmpz_mat_struct"
        let () = F.seal s
        type t = s Ctypes.structure ptr
        let t = ptr s
      end

    module Rand =
      struct
        type s
        let s : s Ctypes.structure typ = typedef (F.structure "flint_rand_s") "flint_rand_s"
        let () = F.seal s
        type t = s Ctypes.structure ptr
        let t = ptr s
      end

    module Fmpz_poly =
      struct
        type s
        let s : s Ctypes.structure F.typ = typedef (F.structure "fmpz_poly_struct") "fmpz_poly_struct"
        let () = F.seal s
        type t = s Ctypes.structure ptr
        let t = ptr s
      end

    module Fmpz_poly_mat =
      struct
        type s
        let s : s Ctypes.structure F.typ = typedef (F.structure "fmpz_poly_mat_struct") "fmpz_poly_mat_struct"
        let ()  = F.seal s
        type t = s Ctypes.structure ptr
        let t = ptr s
      end

    module Mpoly =
      struct
        module Ordering =
          struct
            type t = LEX| DEGLEX | DEGREVLEX
            let lex = F.constant "ORD_LEX" F.int64_t
            let deglex = F.constant "ORD_DEGLEX" F.int64_t
            let degrevlex = F.constant "ORD_DEGREVLEX" F.int64_t
            let t = F.enum ~typedef:true "ordering_t" [ LEX,lex
                                                      ; DEGLEX,deglex
                                                      ; DEGREVLEX,degrevlex]
           end

        module Ctx =
          struct
            type s
            let s : s Ctypes.structure F.typ = typedef (F.structure "mpoly_ctx_struct") "mpoly_ctx_struct"
            let () = F.seal s
            type t = s Ctypes.structure ptr
            let t = ptr s
          end
      end

    module Fmpz_mpoly =
      struct
        module Ordering = Mpoly.Ordering
        module Ctx =
          struct
            type s
            let s : s Ctypes.structure F.typ = typedef (F.structure "fmpz_mpoly_ctx_struct") "fmpz_mpoly_ctx_struct"
            let () = F.seal s
            type t = s Ctypes.structure ptr
            let t = ptr s
          end

        type s
        let s : s Ctypes.structure F.typ = typedef (F.structure "fmpz_mpoly_struct") "fmpz_mpoly_struct"
        let () = F.seal s
        type t = s Ctypes.structure ptr
        let t = ptr s

      end
  end
