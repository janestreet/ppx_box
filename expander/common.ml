open! Ppxlib
open! Stdppx

let raise_unsupported loc ~why =
  Location.raise_errorf ~loc "Unsupported use of ppx_box: %s." why
;;

let lident loc name = Loc.make ~loc (Lident name)
let identifiable_fields = List.map ~f:(fun (label, type_) -> label, gen_symbol (), type_)
let box = "box"
let unbox = "unbox"
let boxed = "boxed"
