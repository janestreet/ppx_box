open! Ppxlib
open! Stdppx
open Ast_builder.Default

module T = struct
  type t = { fields : label_declaration list }

  let parts { fields } loc pattern expression ~type_name ~type_suffix ~params
    : Expander.parts
    =
    let fields =
      List.map fields ~f:(fun label_declaration -> label_declaration.pld_name.txt)
    in
    let patterns =
      List.map fields ~f:(fun name ->
        Common.lident loc name, ppat_var ~loc (Loc.make ~loc name))
    in
    let expressions =
      List.map fields ~f:(fun name ->
        let lident = Common.lident loc name in
        lident, pexp_ident ~loc lident)
    in
    { pattern = pattern ~loc patterns Closed
    ; expression = expression ~loc expressions None
    ; type_ = ptyp_constr ~loc (Common.lident loc (type_name ^ type_suffix)) params
    }
  ;;

  let boxed t loc = parts t loc ppat_record pexp_record ~type_suffix:""

  let unboxed t loc =
    let attrs = [] in
    parts
      t
      loc
      (Ppxlib_jane.Ast_builder.Default.ppat_record_unboxed_product ~attrs)
      (Ppxlib_jane.Ast_builder.Default.pexp_record_unboxed_product ~attrs)
      ~type_suffix:"#"
  ;;
end

include Expander.Make (T)
include T
