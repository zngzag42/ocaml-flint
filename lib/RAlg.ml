module type S =
  sig
    type t
    module R : Ring.S
    module Index : Index.S
    include Ring.S with type t := t
    include RMod.S with type t := t with module R := R with module Index := Index
  end
