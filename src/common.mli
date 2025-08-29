open! Ppxlib
open! Stdppx

val raise_unsupported : location -> why:label -> _
val lident : location -> label -> longident loc

(** identify tuple fields *)
val identifiable_fields
  :  (label option * core_type) list
  -> (label option * label * core_type) list

val box : label
val unbox : label
