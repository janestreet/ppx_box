open! Ppxlib
open! Stdppx

type t =
  { fields :
      (string option
      * string (*_ even without a label we want an identifier for the field *)
      * core_type)
        list
  ; type_declaration_is_unboxed : bool
  }

include Expander.S with type t := t

val extensions : Extension.t list
