open Core
open AbstractSyntaxTree
open Or_error.Let_syntax
open Type
open Identifier

let rec app_to_call_head args exp =
  match exp.exp_desc with
  | E_app (lhs, rhs) -> app_to_call_head (rhs :: args) lhs
  | E_var v_id -> mknoloc_exp @@ E_call (v_id, args)
  | _ -> failwith "[app_to_call_head] not suppor"
;;

let rec app_to_call exp =
  match exp.exp_desc with
  | E_var id -> mknoloc_exp @@ E_var id
  | E_abs (id, ty, body) -> mknoloc_exp @@ E_abs (id, ty, app_to_call body)
  | E_let (e, id, body) -> mknoloc_exp @@ E_let (app_to_call e, id, app_to_call body)
  | E_cond (econd, etrue, efalse) ->
    mknoloc_exp @@ E_cond (app_to_call econd, app_to_call etrue, app_to_call efalse)
  | E_binop (bop, lhs, rhs) ->
    mknoloc_exp @@ E_binop (bop, app_to_call lhs, app_to_call rhs)
  | E_app (lhs, rhs) -> app_to_call_head [ app_to_call rhs ] lhs
  | E_logPr (dist, v) -> mknoloc_exp @@ E_logPr (app_to_call dist, app_to_call v)
  | E_dist (D_normal (loc, scale)) ->
    mknoloc_exp @@ E_dist (D_normal (app_to_call loc, app_to_call scale))
  | E_dist (D_ber p) -> mknoloc_exp @@ E_dist (D_ber (app_to_call p))
  | E_dist (D_exp lambda) -> mknoloc_exp @@ E_dist (D_exp (app_to_call lambda))
  | E_dist (D_pois lambda) -> mknoloc_exp @@ E_dist (D_pois (app_to_call lambda))
  | E_dist (D_gamma (concentration, rate)) ->
    mknoloc_exp @@ E_dist (D_normal (app_to_call concentration, app_to_call rate))
  | E_dist (D_cat logits) -> mknoloc_exp @@ E_dist (D_cat (app_to_call logits))
  | E_dist (D_beta (alpha, beta)) -> mknoloc_exp @@ E_dist (D_beta (app_to_call alpha, app_to_call beta))
  | E_dist (D_geo p) -> mknoloc_exp @@ E_dist (D_geo (app_to_call p))
  | E_dist _ ->
    Format.printf "%a@\n" print_exp exp;
    failwith "[app_to_call] E_dist"
  | E_logML m -> mknoloc_exp @@ E_logML (app_to_call_cmd m)
  | _ -> exp

and app_to_call_cmd cmd =
  match cmd.cmd_desc with
  | M_trm t -> mknoloc_cmd @@ M_trm (app_to_call_trm t)
  | M_bnd (id, t, m) -> mknoloc_cmd @@ M_bnd (id, app_to_call_trm t, app_to_call_cmd m)
  | M_seq (t, m) -> mknoloc_cmd @@ M_seq (app_to_call_trm t, app_to_call_cmd m)

and app_to_call_trm trm =
  match trm.trm_desc with
  | T_ret e -> mknoloc_trm @@ T_ret (app_to_call e)
  | T_factor e -> mknoloc_trm @@ T_factor (app_to_call e)
  | T_sample e -> mknoloc_trm @@ T_sample (app_to_call e)
  | _ ->
    Format.printf "%a@\n" print_trm trm;
    failwith "[app_to_call_trm] todo"
;;

let rec pure_to_func_lambda globals locals top_name exp ty =
  match exp.exp_desc, ty.bty_desc with
  | E_abs (arg_name, arg_type, body), Bty_arrow (_, tail) ->
    let locals' = locals @ [ arg_name, arg_type ] in
    pure_to_func_lambda globals locals' top_name body tail
  | E_abs (_, _, _), _ -> failwith "[pure_to_func_lambda] incorrect type annotations"
  | _, _ ->
    let func_sig = { psig_arg_tys = locals; psig_ret_ty = ty } in
    let func_def = { func_sig; func_body = app_to_call exp; func_loc = Location.none } in
    Ok (Top_func (top_name, func_def))
;;

let pure_to_func_top globals top =
  (* Format.printf "%a@." print_top_level top; *)
  match top with
  | Top_pure (name, ty, def) ->
    let res =
      match def.exp_desc, ty.bty_desc with
      | E_abs _, Bty_arrow _ -> pure_to_func_lambda globals [] name def ty
      | _ -> failwith "[pure_to_func_lambda] incorrect type annotations"
    in
    Some res
  | _ -> Some (Ok top)
;;

let pure_to_func prog =
  let%bind top_external_pure_sigvs = Typing.collect_external_pures prog in
  let%bind top_proc_sigvs = Typing.collect_proc_sigs prog in
  let%bind top_pure_sigvs = Typing.collect_pure_sigs prog in
  let global = { Type.top_external_pure_sigvs; top_proc_sigvs; top_pure_sigvs } in
  let results = List.filter_map prog ~f:(pure_to_func_top global) in
  let succed, failed = List.partition_result results in
  if List.length failed > 0 then Error (List.hd_exn failed) else Ok succed
;;
