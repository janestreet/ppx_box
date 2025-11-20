open! Ppxlib
open! Stdppx

let deriving
  :  location -> type_declaration list -> record:(Record.t, 'output) Expander.t
  -> tuple:(Tuple.t, 'output) Expander.t -> 'output
  =
  fun loc type_declaration ~record ~tuple ->
  List.concat_map type_declaration ~f:(fun td ->
    let { ptype_name; ptype_params; ptype_kind; ptype_manifest; _ } =
      name_type_params_in_td td
    in
    let shim_kind = Ppxlib_jane.Shim.Type_kind.of_parsetree ptype_kind in
    let shim_type =
      Option.map ptype_manifest ~f:Ppxlib_jane.Shim.Core_type.of_parsetree
    in
    let params =
      List.map ~f:(fun (type_, (_ : variance * injectivity)) -> type_) ptype_params
    in
    let type_name = ptype_name.txt in
    match shim_kind, shim_type with
    | Ptype_record fields, _ -> record { fields } loc ~type_name ~params
    | _, Some { ptyp_desc = Ptyp_unboxed_tuple fields; _ } ->
      let fields = Common.identifiable_fields fields in
      tuple { fields; type_declaration_is_unboxed = true } loc ~type_name ~params
    | _, Some { ptyp_desc = Ptyp_tuple fields; _ } ->
      let fields = Common.identifiable_fields fields in
      tuple { fields; type_declaration_is_unboxed = false } loc ~type_name ~params
    | Ptype_record_unboxed_product _, _ ->
      Common.raise_unsupported
        loc
        ~why:
          ("type must be a record, a tuple or an unboxed tuple.\n\
            Hint: this is an unboxed rather than a boxed record.\n\
            Consider changing the record definition to be boxed, then using \""
           ^ ptype_name.txt
           ^ "#\" to refer to the unboxed version")
    | _ ->
      Common.raise_unsupported
        loc
        ~why:"type must be a record, a tuple or an unboxed tuple")
;;

(* deriving *)

let () =
  let generate ~record ~tuple =
    Deriving.Generator.make_noarg
      (fun ~loc ~path:(_ : label) ((_ : rec_flag), type_declaration) ->
         deriving loc type_declaration ~record ~tuple)
  in
  let str_type_decl =
    generate ~record:Record.structure_items ~tuple:Tuple.structure_items
  in
  let sig_type_decl =
    generate ~record:Record.signature_items ~tuple:Tuple.signature_items
  in
  Deriving.add Common.box ~str_type_decl ~sig_type_decl |> Deriving.ignore
;;

(* extension *)

let () = Driver.register_transformation Common.box ~extensions:Tuple.extensions
