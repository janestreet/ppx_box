open! Ppxlib
open! Stdppx
open Ast_builder.Default
include Expander_intf
module Monomorphize = Ppx_template_expander.Monomorphize

let with_suffix loc s ~type_name ~f =
  (match Ppx_helpers.demangle_template type_name with
   | "t", mangle -> s ^ mangle
   | type_name, mangle -> s ^ "_" ^ type_name ^ mangle)
  |> f ~loc
;;

let ptyp_poly loc ~params =
  if List.is_empty params
  then Fn.id
  else
    Ppxlib_jane.Ast_builder.Default.ptyp_poly
      ~loc
      (List.map ~f:Ppxlib_jane.get_type_param_name_and_jkind_of_core_type params)
;;

module Arrow = struct
  type t =
    { box : core_type
    ; unbox : core_type
    }

  let create loc ~boxed ~unboxed ~params =
    let ptyp_poly = ptyp_poly loc ~params in
    let boxed = boxed.type_
    and unboxed = unboxed.type_ in
    { box = ptyp_poly [%type: [%t unboxed] @ m -> [%t boxed] @ m]
    ; unbox = ptyp_poly [%type: [%t boxed] @ m -> [%t unboxed] @ m]
    }
  ;;
end

module Witness = struct
  let type_ loc ~(boxed : parts) ~(unboxed : parts) ~params =
    ptyp_poly
      loc
      ~params
      [%type: unit -> ([%t boxed.type_], [%t unboxed.type_]) Ppx_box_lib.Boxed.t]
  ;;

  let structure loc ~(boxed : parts) ~(unboxed : parts) ~type_name ~params =
    let pvar s = with_suffix loc s ~type_name ~f:pvar in
    let evar s = with_suffix loc s ~type_name ~f:evar in
    let type_declaration (name, manifest) =
      type_declaration
        ~loc
        ~name:(Loc.make ~loc name)
        ~params:(List.map params ~f:(fun type_ -> type_, (NoVariance, NoInjectivity)))
        ~cstrs:[]
        ~kind:Ptype_abstract
        ~private_:Public
        ~manifest:(Some manifest.type_)
    in
    let type_ = type_ loc ~boxed ~unboxed ~params in
    let mod_ =
      pmod_structure
        ~loc
        [%str
          [%%i
            pstr_type
              ~loc
              Nonrecursive
              (List.map ~f:type_declaration [ "t", boxed; "u", unboxed ])]

          let box u = ([%e evar Common.box] [@alloc a]) u [@exclave_if_stack a]
          [@@alloc a = (heap, stack)]
          ;;

          let unbox t =
            ([%e evar Common.unbox] [@mode m] [@zero_alloc assume_unless_opt])
              t [@exclave_if_local m]
          [@@mode m = (global, local)]
          ;;]
    in
    match List.length params with
    | 0 ->
      [%str
        open struct
          module Arg = [%m mod_]
        end

        let [%p pvar Common.boxed] : [%t type_] =
          fun () -> Ppx_box_lib.Boxed.unsafe_create (module Arg)
        ;;]
    | arity when arity < 6 ->
      let payload =
        List.map params ~f:(fun param ->
          match Ppxlib_jane.get_type_param_name_and_jkind_of_core_type param with
          | _, None -> [%expr (_ : (_ : value))]
          | _, jkind ->
            [%expr
              (_
               : [%t
                   { param with
                     ptyp_desc =
                       Ppxlib_jane.Shim.Core_type_desc.to_parsetree (Ptyp_any jkind)
                   }])])
        |> function
        | [] -> assert false
        | [ param ] -> param
        | param :: params ->
          pexp_apply ~loc param (List.map params ~f:(fun param -> Nolabel, param))
      in
      let kind_attribute =
        attribute
          ~loc
          ~name:(Loc.make ~loc "kind.explicit")
          ~payload:(PStr [%str [%e payload]])
      in
      [%str
        open
          [%m
          pmod_apply
            ~loc
            { (pmod_ident
                 ~loc
                 (Loc.make
                    ~loc
                    (Longident.parse
                       (Printf.sprintf "Ppx_box_lib.Boxed.Unsafe_create%d" arity))))
              with
              pmod_attributes = [ kind_attribute ]
            }
            mod_]

        let [%p pvar Common.boxed] : [%t type_] = fun () -> boxed ()]
    | _ -> [%str]
  ;;
end

module Make (X : X) : S with type t = X.t = struct
  include X

  let structure_items x loc ~type_name ~params =
    let pvar s = with_suffix loc s ~type_name ~f:pvar in
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed ~unboxed ~params in
    let witness = Witness.structure loc ~boxed ~unboxed ~type_name ~params in
    [%str
      let [%p pvar Common.box] : [%t arrow.box] =
        fun [%p unboxed.pattern] -> [%e boxed.expression] [@exclave_if_stack a]
      [@@alloc a @ m = (heap_global, stack_local)]
      ;;

      let [%p pvar Common.unbox] : [%t arrow.unbox] =
        fun [%p boxed.pattern] -> [%e unboxed.expression]
      [@@mode m = (global, local)]
      ;;

      include [%m pmod_structure ~loc witness]]
    (* We directly expand the templated code so that the deriving ppx ignores all values,
       instead of just ignoring the value written concretely and forgetting about the
       templated values.
    *)
    |> Monomorphize.t_no_inline#structure Monomorphize.Context.top
  ;;

  let mode_attribute loc =
    attribute ~loc ~name:(Loc.make ~loc "mode") ~payload:(PStr [%str m = (global, local)])
  ;;

  let alloc_attribute loc =
    attribute
      ~loc
      ~name:(Loc.make ~loc "alloc")
      ~payload:(PStr [%str a @ m = (heap_global, stack_local)])
  ;;

  let zero_alloc_attribute loc =
    attribute ~loc ~name:(Loc.make ~loc "zero_alloc") ~payload:(PStr [])
  ;;

  let zero_alloc_if_stack_attribute loc =
    attribute ~loc ~name:(Loc.make ~loc "zero_alloc_if_stack") ~payload:(PStr [%str a])
  ;;

  let signature_item loc name attributes ~type_ ~type_name =
    let name = with_suffix loc name ~type_name ~f:Loc.make in
    let value_desc =
      Ppxlib_jane.Ast_builder.Default.value_description
        ~loc
        ~name
        ~type_
        ~modalities:[ { txt = Modality "stateless"; loc } ]
        ~prim:[]
    in
    { value_desc with pval_attributes = List.map attributes ~f:(( |> ) loc) }
    |> psig_value ~loc
  ;;

  let signature_items x loc ~type_name ~params =
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed ~unboxed ~params in
    let witness_type = Witness.type_ loc ~boxed ~unboxed ~params in
    [ [%sigi:
        [%%template:
        [%%i
          signature_item
            loc
            Common.box
            [ alloc_attribute; zero_alloc_if_stack_attribute ]
            ~type_:arrow.box
            ~type_name]

        [%%i
          signature_item
            loc
            Common.unbox
            [ mode_attribute; zero_alloc_attribute ]
            ~type_:arrow.unbox
            ~type_name]

        [%%i
          if List.length params < 6
          then
            signature_item
              loc
              Common.boxed
              [ zero_alloc_attribute ]
              ~type_:witness_type
              ~type_name
          else [%sigi: include sig end]]]]
    ]
  ;;
end
