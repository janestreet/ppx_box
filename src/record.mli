open! Ppxlib
open! Stdppx

type t = { fields : label_declaration list }

include Expander.S with type t := t
