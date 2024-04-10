open Core
open AbstractSyntaxTree

let rec eta_conversion_exp exp =
  (* Format.printf "[eta_conversion_exp]@.%a@." print_exp exp; *)
  match exp.exp_desc with
  | E_abs (id, _, { exp_desc = E_app (rator, { exp_desc = E_var rand; _ }); _ }) ->
    if String.equal id.txt rand.txt then eta_conversion_exp rator else exp
  | E_call (id, [ { exp_desc = E_var rand; _ } ]) ->
    (* Format.printf "!!!!!%a@\n" print_exp exp; *)
    if String.equal id.txt rand.txt then mknoloc_exp @@ E_var id else exp
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
  | E_logML m -> mknoloc_exp @@ E_logML (eta_conversion_cmd m)
  | _ -> exp

and eta_conversion_trm trm =
  match trm.trm_desc with
  | T_factor e -> mknoloc_trm @@ T_factor (eta_conversion_exp e)
  | T_ret e -> mknoloc_trm @@ T_ret (eta_conversion_exp e)
  | T_sample e -> mknoloc_trm @@ T_sample (eta_conversion_exp e)
  | _ -> failwith "eta_conversion_trm TODO"

and eta_conversion_cmd cmd =
  match cmd.cmd_desc with
  | M_trm t -> mknoloc_cmd @@ M_trm (eta_conversion_trm t)
  | M_seq (t, m) -> mknoloc_cmd @@ M_seq (eta_conversion_trm t, eta_conversion_cmd m)
  | M_bnd (id, t, m) ->
    mknoloc_cmd @@ M_bnd (id, eta_conversion_trm t, eta_conversion_cmd m)
;;

let eta_conversion_top = function
  | Top_pure (name, ty, def) -> Top_pure (name, ty, eta_conversion_exp def)
  | top -> top
;;

let eta_conversion_prog prog = List.map prog ~f:eta_conversion_top
