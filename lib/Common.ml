open Core

module Location_ext = struct
  include Location

  let pp = Location.print_loc

  let equal lhs rhs =
    let lhs_s_flc = Location.get_pos_info lhs.loc_start in
    let rhs_s_flc = Location.get_pos_info rhs.loc_start in
    let lhs_e_flc = Location.get_pos_info lhs.loc_end in
    let rhs_e_flc = Location.get_pos_info rhs.loc_end in
    [%equal: (string * int * int) * (string * int * int)]
      (lhs_s_flc, rhs_s_flc)
      (lhs_e_flc, rhs_e_flc)
  ;;

  let compare lhs rhs =
    let lhs_s_flc = Location.get_pos_info lhs.loc_start in
    let rhs_s_flc = Location.get_pos_info rhs.loc_start in
    let lhs_e_flc = Location.get_pos_info lhs.loc_end in
    let rhs_e_flc = Location.get_pos_info rhs.loc_end in
    [%compare: (string * int * int) * (string * int * int)]
      (lhs_s_flc, rhs_s_flc)
      (lhs_e_flc, rhs_e_flc)
  ;;

  let hash_fold_t h loc =
    let s_flc = Location.get_pos_info loc.loc_start in
    let e_flc = Location.get_pos_info loc.loc_end in
    [%hash_fold: (string * int * int) * (string * int * int)] h (s_flc, e_flc)
  ;;
end

let print_line_column fmt pos =
  let _, line, column = Location.get_pos_info pos in
  Format.fprintf fmt "L%dC%d" line column
;;

let print_comma_sep fmt () = Format.fprintf fmt ", "
let print_semi_sep fmt () = Format.fprintf fmt "; "
let print_list ~f = Format.pp_print_list ~pp_sep:print_comma_sep f
