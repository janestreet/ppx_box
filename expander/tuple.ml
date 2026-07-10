open! Ppxlib
open! Stdppx
open Ast_builder.Default

module T = struct
  type t =
    { fields : (string option * string * core_type) list
    ; type_declaration_is_unboxed : bool
    }

  let map_snd (a, b) ~f = a, f b

  type maybe_anon =
    | Anon
    | Named of string

  let parts fields loc maybe_anon ~unboxed ~type_declaration_is_unboxed ~params
    : Expander.parts
    =
    let pattern =
      if unboxed
      then Ppxlib_jane.Ast_builder.Default.ppat_unboxed_tuple
      else Ppxlib_jane.Ast_builder.Default.ppat_tuple
    in
    let expression =
      if unboxed
      then Ppxlib_jane.Ast_builder.Default.pexp_unboxed_tuple
      else Ppxlib_jane.Ast_builder.Default.pexp_tuple
    in
    let fields' =
      List.map fields ~f:(fun (label, name, (_ : core_type)) -> label, name)
    in
    let patterns =
      List.map fields' ~f:(map_snd ~f:(fun name -> ppat_var ~loc (Loc.make ~loc name)))
    in
    let expressions =
      List.map
        fields'
        ~f:(map_snd ~f:(fun name -> pexp_ident ~loc (Common.lident loc name)))
    in
    let attrs = None in
    { pattern = pattern ~loc ?attrs patterns Closed
    ; expression = expression ~loc ?attrs expressions
    ; type_ =
        (match maybe_anon with
         (* We use the type name iff the boxing of the type declaration matches the boxing
            of the type expression we're creating *)
         | Named type_name when Bool.equal unboxed type_declaration_is_unboxed ->
           ptyp_constr ~loc (Common.lident loc type_name) params
         | Named _ | Anon ->
           let fields' =
             List.map fields ~f:(fun (label, (_ : string), type_) -> label, type_)
           in
           if unboxed
           then Ppxlib_jane.Ast_builder.Default.ptyp_unboxed_tuple ~loc fields'
           else Ppxlib_jane.Ast_builder.Default.ptyp_tuple ~loc fields')
    }
  ;;

  (* for use in extensions *)
  let boxed_internal fields loc maybe_anon ~type_declaration_is_unboxed =
    parts fields loc maybe_anon ~type_declaration_is_unboxed ~unboxed:false
  ;;

  let unboxed_internal fields loc maybe_anon ~type_declaration_is_unboxed =
    parts fields loc maybe_anon ~type_declaration_is_unboxed ~unboxed:true
  ;;

  let boxed t loc ~type_name =
    boxed_internal
      t.fields
      loc
      (Named type_name)
      ~type_declaration_is_unboxed:t.type_declaration_is_unboxed
  ;;

  let unboxed t loc ~type_name =
    unboxed_internal
      t.fields
      loc
      (Named type_name)
      ~type_declaration_is_unboxed:t.type_declaration_is_unboxed
  ;;
end

include Expander.Make (T)
include T

let parts loc fields ~type_declaration_is_unboxed =
  let fields = Common.identifiable_fields fields in
  let params = [] in
  ( boxed_internal fields loc Anon ~params ~type_declaration_is_unboxed
  , unboxed_internal fields loc Anon ~params ~type_declaration_is_unboxed )
;;

let box loc type_ ~f =
  match Ppxlib_jane.Shim.Core_type.of_parsetree type_ with
  | { ptyp_desc = Ptyp_unboxed_tuple fields; _ } ->
    let boxed, unboxed = parts loc fields ~type_declaration_is_unboxed:true in
    f loc ~boxed ~unboxed
  | _ -> Common.raise_unsupported loc ~why:"box extension only works with unboxed tuples"
;;

let unbox loc type_ ~f =
  match Ppxlib_jane.Shim.Core_type.of_parsetree type_ with
  | { ptyp_desc = Ptyp_tuple fields; _ } ->
    let boxed, unboxed = parts loc fields ~type_declaration_is_unboxed:false in
    f loc ~boxed ~unboxed
  | _ -> Common.raise_unsupported loc ~why:"unbox extension only works with boxed tuples"
;;

let extension name ~f =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(ptyp __)
    (fun ~loc ~path:(_ : string) type_ ->
      f (Ppx_helpers.ghoster#location loc) (Ppx_helpers.ghoster#core_type type_))
;;

let extensions =
  [ extension
      Common.box
      ~f:
        (box ~f:(fun loc ~boxed ~unboxed ->
           [%expr
             fun ([%p unboxed.pattern] : [%t unboxed.type_]) : [%t boxed.type_] ->
               [%e boxed.expression]]))
  ; extension
      (Common.box ^ "__stack")
      ~f:
        (box ~f:(fun loc ~boxed ~unboxed ->
           [%expr
             fun ([%p unboxed.pattern] : [%t unboxed.type_] @ local) : [%t boxed.type_] -> exclave_
               [%e boxed.expression]]))
  ; extension
      Common.unbox
      ~f:
        (unbox ~f:(fun loc ~boxed ~unboxed ->
           [%expr
             fun ([%p boxed.pattern] : [%t boxed.type_]) : [%t unboxed.type_] ->
               [%e unboxed.expression]]))
  ; extension
      (Common.unbox ^ "__local")
      ~f:
        (unbox ~f:(fun loc ~boxed ~unboxed ->
           [%expr
             fun ([%p boxed.pattern] : [%t boxed.type_] @ local) : [%t unboxed.type_] -> exclave_
               [%e unboxed.expression]]))
  ]
;;
