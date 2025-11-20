module%template Definitions = struct
  module type S = sig @@ stateless
    type t : value
    type u : any

    val box : u @ l -> t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox : t @ l -> u @ l [@@zero_alloc] [@@mode l = (global, local)]
  end

  [@@@kind.default.explicit ka = (value, value_or_null_with_imm)]

  module type S1 = sig @@ stateless
    type ('a : ka) t : value
    type ('a : ka) u : any

    val box : ('a : ka). 'a u @ l -> 'a t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox : ('a : ka). 'a t @ l -> 'a u @ l
    [@@zero_alloc] [@@mode l = (global, local)]
  end

  [@@@kind.default.explicit kb = (value, value_or_null_with_imm)]

  module type S2 = sig @@ stateless
    type ('a : ka, 'b : kb) t : value
    type ('a : ka, 'b : kb) u : any

    val box : ('a : ka) ('b : kb). ('a, 'b) u @ l -> ('a, 'b) t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox : ('a : ka) ('b : kb). ('a, 'b) t @ l -> ('a, 'b) u @ l
    [@@zero_alloc] [@@mode l = (global, local)]
  end

  [@@@kind.default.explicit kc = (value, value_or_null_with_imm)]

  module type S3 = sig @@ stateless
    type ('a : ka, 'b : kb, 'c : kc) t : value
    type ('a : ka, 'b : kb, 'c : kc) u : any

    val box : ('a : ka) ('b : kb) ('c : kc). ('a, 'b, 'c) u @ l -> ('a, 'b, 'c) t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox : ('a : ka) ('b : kb) ('c : kc). ('a, 'b, 'c) t @ l -> ('a, 'b, 'c) u @ l
    [@@zero_alloc] [@@mode l = (global, local)]
  end

  [@@@kind.default.explicit kd = (value, value_or_null_with_imm)]

  module type S4 = sig @@ stateless
    type ('a : ka, 'b : kb, 'c : kc, 'd : kd) t : value
    type ('a : ka, 'b : kb, 'c : kc, 'd : kd) u : any

    val box
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd).
      ('a, 'b, 'c, 'd) u @ l -> ('a, 'b, 'c, 'd) t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd).
      ('a, 'b, 'c, 'd) t @ l -> ('a, 'b, 'c, 'd) u @ l
    [@@zero_alloc] [@@mode l = (global, local)]
  end

  [@@@kind.default.explicit ke = (value, value_or_null_with_imm)]

  module type S5 = sig @@ stateless
    type ('a : ka, 'b : kb, 'c : kc, 'd : kd, 'e : ke) t : value
    type ('a : ka, 'b : kb, 'c : kc, 'd : kd, 'e : ke) u : any

    val box
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd) ('e : ke).
      ('a, 'b, 'c, 'd, 'e) u @ l -> ('a, 'b, 'c, 'd, 'e) t @ l
    [@@zero_alloc_if_stack a] [@@alloc a @ l = (heap @ global, stack @ local)]

    val unbox
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd) ('e : ke).
      ('a, 'b, 'c, 'd, 'e) t @ l -> ('a, 'b, 'c, 'd, 'e) u @ l
    [@@zero_alloc] [@@mode l = (global, local)]
  end
end

module type Boxed = sig @@ stateless
  include module type of struct
    include Definitions
  end

  (** An [('a, 'b) t] is a witness that ['a] _boxes_ ['b]. That is, a value of type ['a]
      is represented in memory as an OCaml block, with a valid header, containing a single
      ['b].

      In particular, for any record type [r] and implicit unboxed record type [r#], it is
      possible to produce a [(r, r#) t], serving as a witness that [r] boxes [r#].

      For most types, one should use ppx_box to obtain a witness, e.g.
      {[
        type r =
          { fst : f64
          ; snd : i64
          }
        [@@deriving_inline box]

        val box : r# @ l -> r @ l
        [@@alloc a @ l = (heap @ global, stack @ local)] [@@zero_alloc_if_stack a]

        val unbox : r @ l -> r# @ l [@@mode l = (global, local)] [@@zero_alloc]
        val boxed : unit -> (r, r#) Ox.Boxed.t [@@zero_alloc]

        [@@@end]
      ]}

      In the future, we expect OxCaml to have a built-in type constructor [(_ : any) box],
      such that the type equality [r = r# box] holds. *)
  type ('a : value, 'b : any) t : void

  (** Think twice about calling this yourself! Most instances of [t] should be generated
      by ppx_box. Just because it is _possible_ to implement [box] and [unbox] does NOT
      necessarily mean that it is safe to create a witness for two types.

      In particular, it would be a mistake to create a witness for basically all of the
      unboxed number types! *)
  val unsafe_create
    : ('a : value) ('b : any).
    (module S with type t = 'a and type u = 'b) -> ('a, 'b) t
  [@@zero_alloc]

  [%%template:
  [@@@kind.default.explicit ka = (value, value_or_null_with_imm)]

  module Unsafe_create1 (M : S1 [@kind.explicit ka]) : sig @@ stateless
    val boxed : ('a : ka). unit -> ('a M.t, 'a M.u) t [@@zero_alloc]
  end

  [@@@kind.default.explicit kb = (value, value_or_null_with_imm)]

  module Unsafe_create2 (M : S2 [@kind.explicit ka kb]) : sig @@ stateless
    val boxed : ('a : ka) ('b : kb). unit -> (('a, 'b) M.t, ('a, 'b) M.u) t [@@zero_alloc]
  end

  [@@@kind.default.explicit kc = (value, value_or_null_with_imm)]

  module Unsafe_create3 (M : S3 [@kind.explicit ka kb kc]) : sig @@ stateless
    val boxed
      : ('a : ka) ('b : kb) ('c : kc).
      unit -> (('a, 'b, 'c) M.t, ('a, 'b, 'c) M.u) t
    [@@zero_alloc]
  end

  [@@@kind.default.explicit kd = (value, value_or_null_with_imm)]

  module Unsafe_create4 (M : S4 [@kind.explicit ka kb kc kd]) : sig @@ stateless
    val boxed
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd).
      unit -> (('a, 'b, 'c, 'd) M.t, ('a, 'b, 'c, 'd) M.u) t
    [@@zero_alloc]
  end

  [@@@kind.default.explicit ke = (value, value_or_null_with_imm)]

  module Unsafe_create5 (M : S5 [@kind.explicit ka kb kc kd ke]) : sig @@ stateless
    val boxed
      : ('a : ka) ('b : kb) ('c : kc) ('d : kd) ('e : ke).
      unit -> (('a, 'b, 'c, 'd, 'e) M.t, ('a, 'b, 'c, 'd, 'e) M.u) t
    [@@zero_alloc]
  end]
end
