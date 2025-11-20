include Boxed_intf.Definitions

type ('a : value, 'b : any) t : void

external make : ('a : value) ('b : any). unit -> ('a, 'b) t @@ stateless = "%unbox_unit"

let unsafe_create
  : ('a : value) ('b : any). (module S with type t = 'a and type u = 'b) -> ('a, 'b) t
  =
  fun _ -> make ()
;;

[%%template
[@@@kind.default.explicit ka = (value, value_or_null_with_imm)]

module Unsafe_create1 (M : S1 [@kind.explicit ka]) = struct
  let boxed : ('a : ka). unit -> ('a M.t, 'a M.u) t = [%eta1 make]
end

[@@@kind.default.explicit kb = (value, value_or_null_with_imm)]

module Unsafe_create2 (M : S2 [@kind.explicit ka kb]) = struct
  let boxed : ('a : ka) ('b : kb). unit -> (('a, 'b) M.t, ('a, 'b) M.u) t = [%eta1 make]
end

[@@@kind.default.explicit kc = (value, value_or_null_with_imm)]

module Unsafe_create3 (M : S3 [@kind.explicit ka kb kc]) = struct
  let boxed
    : ('a : ka) ('b : kb) ('c : kc). unit -> (('a, 'b, 'c) M.t, ('a, 'b, 'c) M.u) t
    =
    [%eta1 make]
  ;;
end

[@@@kind.default.explicit kd = (value, value_or_null_with_imm)]

module Unsafe_create4 (M : S4 [@kind.explicit ka kb kc kd]) = struct
  let boxed
    : ('a : ka) ('b : kb) ('c : kc) ('d : kd).
    unit -> (('a, 'b, 'c, 'd) M.t, ('a, 'b, 'c, 'd) M.u) t
    =
    [%eta1 make]
  ;;
end

[@@@kind.default.explicit ke = (value, value_or_null_with_imm)]

module Unsafe_create5 (M : S5 [@kind.explicit ka kb kc kd ke]) = struct
  let boxed
    : ('a : ka) ('b : kb) ('c : kc) ('d : kd) ('e : ke).
    unit -> (('a, 'b, 'c, 'd, 'e) M.t, ('a, 'b, 'c, 'd, 'e) M.u) t
    =
    [%eta1 make]
  ;;
end]
