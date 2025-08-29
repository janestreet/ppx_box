open! Ppxlib
open! Stdppx

type ('context, 'output) t =
  'context -> location -> type_name:string -> params:core_type list -> 'output

type parts =
  { pattern : pattern
  ; expression : expression
  ; type_ : core_type
  }

module type X = sig
  type ('a, 'b) expander := ('a, 'b) t
  type t

  val boxed : (t, parts) expander
  val unboxed : (t, parts) expander
end

module type S = sig
  type ('a, 'b) expander := ('a, 'b) t
  type t

  val structure_items : (t, structure_item list) expander
  val signature_items : (t, signature_item list) expander
end

module type Expander = sig
  type nonrec ('a, 'b) t = ('a, 'b) t
  type nonrec parts = parts

  module type S = S

  module Make : functor (X : X) -> S with type t = X.t
end
