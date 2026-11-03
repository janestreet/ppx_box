open! Ppxlib
open! Stdppx
open Ast_builder.Default

module T = struct
  type t =
    { constr : longident loc
    ; arg : pattern * expression
    }

  (* Call the aliased type's [box]/[unbox] applied to the argument *)
  let delegate ~loc constr prefix arg wrap_attr =
    let fun_ =
      Ppx_helpers.type_constr_conv_expr ~loc constr [] ~f:(fun ?functor_ type_name ->
        match functor_ with
        | Some _ ->
          Common.raise_unsupported
            constr.loc
            ~why:"type alias must not be a functor application"
        | None -> Expander.value_name prefix ~type_name)
      |> wrap_attr
    in
    [%expr [%e fun_] [%e arg]]
  ;;

  let boxed { constr; arg = arg_pat, arg_expr } loc ~type_name ~params : Expander.parts =
    { pattern = arg_pat
    ; expression =
        delegate ~loc constr Common.box arg_expr (fun e -> [%expr [%e e] [@alloc a]])
    ; type_ = ptyp_constr ~loc (Common.lident loc type_name) params
    }
  ;;

  let unboxed { constr; arg = arg_pat, arg_expr } loc ~type_name ~params : Expander.parts =
    (* [unbox] derived on a record can return the components directly because they are
       regional, but here we must use [exclave_] because we are returning the output of a
       [local]-returning function. *)
    let expr =
      delegate ~loc constr Common.unbox arg_expr (fun e -> [%expr [%e e] [@mode l]])
    in
    { pattern = arg_pat
    ; expression = [%expr [%e expr] [@exclave_if_local l]]
    ; type_ =
        (* Types that are manifestly bound to types with implicit unboxed types themselves
           have implicit unboxed types *)
        ptyp_constr ~loc (Common.lident loc (type_name ^ "#")) params
    }
  ;;
end

include Expander.Make (T)
include T
