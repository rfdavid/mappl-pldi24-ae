open Core
open AbstractSyntaxTree

let rec eta_conversion_exp exp =
  (* Format.printf "[eta_conversion_exp]@.%a@." print_exp exp; *)
  match exp.exp_desc with
  | E_abs (id, _, { exp_desc = E_app (rator, { exp_desc = E_var rand; _ }); _ }) ->
    if String.equal id.txt rand.txt then eta_conversion_exp rator else exp
  | E_abs (id, ty, body) -> mknoloc_exp @@ E_abs (id, ty, eta_conversion_exp body)
  | E_let (e, id, body) ->
    mknoloc_exp @@ E_let (eta_conversion_exp e, id, eta_conversion_exp body)
  | E_cond (econd, etrue, efalse) ->
    mknoloc_exp
    @@ E_cond
         (eta_conversion_exp econd, eta_conversion_exp etrue, eta_conversion_exp efalse)
  | E_binop (bop, lhs, rhs) ->
    mknoloc_exp @@ E_binop (bop, eta_conversion_exp lhs, eta_conversion_exp rhs)
  | E_app (lhs, rhs) ->
    mknoloc_exp @@ E_app (eta_conversion_exp lhs, eta_conversion_exp rhs)
  | _ -> exp
;;

let eta_conversion_top = function
  | Top_pure (name, ty, def) -> Top_pure (name, ty, eta_conversion_exp def)
  | top -> top
;;

let eta_conversion_prog prog = List.map prog ~f:eta_conversion_top
