open! Ppxlib
open! Stdppx
open Ast_builder.Default
include Expander_intf
module Monomorphize = Ppx_template_expander.Monomorphize

let with_suffix loc s ~type_name =
  (match Ppx_helpers.demangle_template type_name with
   | "t", mangle -> s ^ mangle
   | type_name, mangle -> s ^ "_" ^ type_name ^ mangle)
  |> Loc.make ~loc
;;

module Arrow = struct
  type t =
    { box : core_type
    ; unbox : core_type
    }

  let create loc ~boxed ~unboxed =
    { box = [%type: [%t unboxed] @ m -> [%t boxed] @ m]
    ; unbox = [%type: [%t boxed] @ m -> [%t unboxed] @ m]
    }
  ;;
end

module Make (X : X) : S with type t = X.t = struct
  include X

  let structure_items x loc ~type_name ~params =
    let ppat_var s = with_suffix loc s ~type_name |> ppat_var ~loc in
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed:boxed.type_ ~unboxed:unboxed.type_ in
    [%str
      let [%p ppat_var Common.box] : [%t arrow.box] =
        fun [%p unboxed.pattern] -> [%e boxed.expression] [@exclave_if_stack a]
      [@@alloc a @ m = (heap_global, stack_local)]
      ;;

      let [%p ppat_var Common.unbox] : [%t arrow.unbox] =
        fun [%p boxed.pattern] -> [%e unboxed.expression]
      [@@mode m = (global, local)]
      ;;]
    (* We directly expand the templated code so that the deriving ppx ignores all values,
       instead of just ignoring the value written concretely and forgetting about the
       templated values.
    *)
    |> Monomorphize.t_no_inline#structure Monomorphize.Context.top
  ;;

  let locality_attribute loc =
    attribute
      ~loc
      ~name:(Loc.make ~loc "mode")
      ~payload:(PStr [ [%stri m = (global, local)] ])
  ;;

  let alloc_attribute loc =
    attribute
      ~loc
      ~name:(Loc.make ~loc "alloc")
      ~payload:(PStr [ [%stri __ @ m = (heap_global, stack_local)] ])
  ;;

  let signature_item loc name attribute ~arrow ~type_name =
    let name = with_suffix loc name ~type_name in
    let value_desc = value_description ~loc ~name ~type_:arrow ~prim:[] in
    { value_desc with pval_attributes = [ attribute loc ] } |> psig_value ~loc
  ;;

  let signature_items x loc ~type_name ~params =
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed:boxed.type_ ~unboxed:unboxed.type_ in
    [ [%sigi:
        [%%template:
        [%%i signature_item loc Common.box alloc_attribute ~arrow:arrow.box ~type_name]

        [%%i
          signature_item loc Common.unbox locality_attribute ~arrow:arrow.unbox ~type_name]]]
    ]
  ;;
end
