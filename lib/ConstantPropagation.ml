open Core
open AbstractSyntaxTree

let rec const_propagation_exp exp =
  (* Format.printf "[const_propagation_exp]@.%a@." print_exp exp; *)
  match exp.exp_desc with
  | E_let (e, id, body) ->
    (match e.exp_desc with
     | E_var _ | E_triv ->
       let s = String.Map.singleton id.txt e in
       let body' = subst_exp s body in
       const_propagation_exp body'
     | _ -> mknoloc_exp @@ E_let (const_propagation_exp e, id, const_propagation_exp body))
  | E_abs (id, ty, body) -> mknoloc_exp @@ E_abs (id, ty, const_propagation_exp body)
  | E_cond (econd, etrue, efalse) ->
    mknoloc_exp
    @@ E_cond
         ( const_propagation_exp econd
         , const_propagation_exp etrue
         , const_propagation_exp efalse )
  | E_binop ({ txt = Bop_add; _ }, lhs, { exp_desc = E_real 0.0; _ }) ->
    const_propagation_exp lhs
  | E_binop (bop, lhs, rhs) ->
    mknoloc_exp @@ E_binop (bop, const_propagation_exp lhs, const_propagation_exp rhs)
  | E_app (lhs, rhs) ->
    mknoloc_exp @@ E_app (const_propagation_exp lhs, const_propagation_exp rhs)
  | E_logML m -> mknoloc_exp @@ E_logML (const_propagation_cmd m)
  | _ -> exp

and const_propagation_trm trm =
  match trm.trm_desc with
  | T_factor e -> mknoloc_trm @@ T_factor (const_propagation_exp e)
  | T_ret e -> mknoloc_trm @@ T_ret (const_propagation_exp e)
  | T_sample e -> mknoloc_trm @@ T_sample (const_propagation_exp e)
  | _ -> failwith "const_propagation_trm TODO"

and const_propagation_cmd cmd =
  match cmd.cmd_desc with
  | M_trm t -> mknoloc_cmd @@ M_trm (const_propagation_trm t)
  | M_seq (t, m) -> mknoloc_cmd @@ M_seq (const_propagation_trm t, const_propagation_cmd m)
  | M_bnd (id, t, m) ->
    mknoloc_cmd @@ M_bnd (id, const_propagation_trm t, const_propagation_cmd m)
;;

let const_propagation_top = function
  | Top_pure (name, ty, def) -> Top_pure (name, ty, const_propagation_exp def)
  | top -> top
;;

let const_propagation_prog prog = List.map prog ~f:const_propagation_top
