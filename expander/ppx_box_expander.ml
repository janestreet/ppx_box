open! Ppxlib
open! Stdppx

let boxed_witness type_ =
  let loc = type_.ptyp_loc in
  match Ppxlib_jane.Shim.Core_type.of_parsetree type_ with
  | { ptyp_desc = Ptyp_constr (id, _); _ } ->
    Ppx_helpers.type_constr_conv_expr
      ~loc
      id
      ~f:(fun ?functor_:_ type_name -> Expander.value_name Common.boxed ~type_name)
      [ [%expr ()] ]
  | _ ->
    Common.raise_unsupported loc ~why:"boxed_witness only works with type constructors"
;;

module Private = struct
  module Common = Common
  module Expander = Expander
  module Record = Record
  module Tuple = Tuple
end
