open! Ppxlib
open! Stdppx
open Ast_builder.Default
include Expander_intf
module Monomorphize = Ppx_template_expander.Monomorphize

let value_name s ~type_name =
  match Ppx_helpers.demangle_template type_name with
  | "t", mangle -> s ^ mangle
  | type_name, mangle -> s ^ "_" ^ type_name ^ mangle
;;

let with_suffix loc s ~type_name ~f = f ~loc (value_name s ~type_name)

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

  let create loc ~boxed ~unboxed ~params ~portable =
    let ptyp_poly = ptyp_poly loc ~params in
    let boxed = boxed.type_
    and unboxed = unboxed.type_ in
    if portable
    then
      { box = ptyp_poly [%type: [%t unboxed] @ l p -> [%t boxed] @ l p]
      ; unbox = ptyp_poly [%type: [%t boxed] @ l p -> [%t unboxed] @ l p]
      }
    else
      { box = ptyp_poly [%type: [%t unboxed] @ l -> [%t boxed] @ l]
      ; unbox = ptyp_poly [%type: [%t boxed] @ l -> [%t unboxed] @ l]
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
          fun () -> Ppx_box_lib.Boxed.magic_create (module Arg)
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

  let portable_mode_attributes ~portable loc =
    if portable
    then
      [ attribute
          ~loc
          ~name:(Loc.make ~loc "mode")
          ~payload:(PStr [%str p = (portable, shareable, nonportable)])
      ]
    else []
  ;;

  let structure_items ~portable x loc ~type_name ~params =
    let pvar s = with_suffix loc s ~type_name ~f:pvar in
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed ~unboxed ~params ~portable in
    let witness = Witness.structure loc ~boxed ~unboxed ~type_name ~params in
    let box =
      { Ppxlib_jane.Ast_builder.Default.(
          value_binding
            ~loc
            ~pat:(ppat_constraint ~loc (pvar Common.box) (Some arrow.box) [])
            ~expr:
              [%expr
                fun [%p unboxed.pattern] -> [%e boxed.expression] [@exclave_if_stack a]]
            ~modes:[])
        with
        pvb_attributes =
          [ attribute
              ~loc
              ~name:(Loc.make ~loc "alloc")
              ~payload:(PStr [%str a @ l = (heap_global, stack_local)])
          ]
          @ portable_mode_attributes ~portable loc
      }
    in
    let unbox_mode_attribute =
      attribute
        ~loc
        ~name:(Loc.make ~loc "mode")
        ~payload:
          (PStr
             (if portable
              then [%str l = (global, local), p = (portable, shareable, nonportable)]
              else [%str l = (global, local)]))
    in
    let unbox =
      { Ppxlib_jane.Ast_builder.Default.(
          value_binding
            ~loc
            ~pat:(ppat_constraint ~loc (pvar Common.unbox) (Some arrow.unbox) [])
            ~expr:[%expr fun [%p boxed.pattern] -> [%e unboxed.expression]]
            ~modes:[])
        with
        pvb_attributes = [ unbox_mode_attribute ]
      }
    in
    [ pstr_value ~loc Nonrecursive [ box ]; pstr_value ~loc Nonrecursive [ unbox ] ]
    @ [%str include [%m pmod_structure ~loc witness]]
    (* We directly expand the templated code so that the deriving ppx ignores all values,
       instead of just ignoring the value written concretely and forgetting about the
       templated values.
    *)
    |> Monomorphize.t#structure Monomorphize.Context.top
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
    { value_desc with pval_attributes = attributes } |> psig_value ~loc
  ;;

  let signature_items ~portable x loc ~type_name ~params =
    let boxed = boxed x loc ~type_name ~params in
    let unboxed = unboxed x loc ~type_name ~params in
    let arrow = Arrow.create loc ~boxed ~unboxed ~params ~portable in
    let witness_type = Witness.type_ loc ~boxed ~unboxed ~params in
    let unbox_mode_attribute =
      attribute
        ~loc
        ~name:(Loc.make ~loc "mode")
        ~payload:
          (PStr
             (if portable
              then [%str l = (global, local), p = (portable, shareable, nonportable)]
              else [%str l = (global, local)]))
    in
    [ [%sigi:
        [%%template:
        [%%i
          signature_item
            loc
            Common.box
            ([ attribute
                 ~loc
                 ~name:(Loc.make ~loc "alloc")
                 ~payload:(PStr [%str a @ l = (heap_global, stack_local)])
             ]
             @ portable_mode_attributes ~portable loc
             @ [ zero_alloc_if_stack_attribute loc ])
            ~type_:arrow.box
            ~type_name]

        [%%i
          signature_item
            loc
            Common.unbox
            [ unbox_mode_attribute; zero_alloc_attribute loc ]
            ~type_:arrow.unbox
            ~type_name]

        [%%i
          if List.length params < 6
          then
            signature_item
              loc
              Common.boxed
              [ zero_alloc_attribute loc ]
              ~type_:witness_type
              ~type_name
          else [%sigi: include sig end]]]]
    ]
  ;;
end
