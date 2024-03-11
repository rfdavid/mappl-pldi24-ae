open Core
open Or_error.Let_syntax
open AbstractSyntaxTree
open Type
open Identifier
open Common

let erase_loc_proc_sig psig =
  { psigv_arg_tys =
      List.map psig.psig_arg_tys ~f:(fun (var_name, ty) ->
        var_name.txt, erase_loc_base_ty ty)
  ; psigv_ret_ty = erase_loc_base_ty psig.psig_ret_ty
  }
;;

let find_proc prog name =
  List.find_map_exn prog ~f:(fun top ->
    match top with
    | Top_proc (proc_name, proc) ->
      if String.equal name proc_name.txt then Some proc else None
    | _ -> None)
;;

let collect_proc_sigs prog =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_proc (proc_name, { proc_sig; _ }) ->
         Some (proc_name.txt, erase_loc_proc_sig proc_sig)
       | _ -> None))
;;

let collect_pure_sigs prog =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_pure (name, bty, _) -> Some (name.txt, erase_loc_base_ty bty)
       | _ -> None))
;;

let collect_external_pures prog =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_external_pure (var_name, ty) -> Some (var_name.txt, erase_loc_base_ty ty)
       | _ -> None))
;;

let print_ctx fmt ctx =
  let kvs = Map.to_alist ctx in
  if List.is_empty kvs then Format.fprintf fmt "." else print_list ~f:print_arg fmt kvs
;;

let rec lookup_in_maps maps ~key =
  match maps with
  | [] -> None
  | map :: rest ->
    (match Map.find map key with
     | Some value -> Some value
     | None -> lookup_in_maps rest ~key)
;;

let lookup_pures { top_external_pure_sigvs; top_pure_sigvs; _ } locals ~key =
  lookup_in_maps [ top_external_pure_sigvs; top_pure_sigvs; locals ] ~key
;;

let is_prim_subtype pty1 pty2 =
  match pty1, pty2 with
  | Pty_unit, Pty_unit -> true
  | Pty_bool, Pty_bool -> true
  | Pty_ureal, Pty_ureal | Pty_ureal, Pty_preal | Pty_ureal, Pty_real -> true
  | Pty_preal, Pty_preal | Pty_preal, Pty_real -> true
  | Pty_real, Pty_real -> true
  | Pty_fnat n, Pty_fnat m -> n <= m
  | Pty_fnat _, Pty_nat -> true
  | Pty_fnat n, Pty_ureal -> n <= 2
  | Pty_fnat _, Pty_preal | Pty_fnat _, Pty_real -> true
  | Pty_nat, Pty_nat | Pty_nat, Pty_preal | Pty_nat, Pty_real -> true
  | _ -> false
;;

let rec is_subtype tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 -> is_prim_subtype pty1 pty2
  | Btyv_var name1, Btyv_var name2 -> String.(name1 = name2)
  | Btyv_sum (tyv11, tyv12), Btyv_sum (tyv21, tyv22) ->
    is_subtype tyv11 tyv21 && is_subtype tyv12 tyv22
  | Btyv_prod (tyv11, tyv12), Btyv_prod (tyv21, tyv22) ->
    is_subtype tyv11 tyv21 && is_subtype tyv12 tyv22
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    is_subtype tyv21 tyv11 && is_subtype tyv12 tyv22
  | Btyv_dist tyv1', Btyv_dist tyv2' -> equal_base_tyv tyv1' tyv2'
  | _ -> false
;;

let join_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2
  then Ok pty2
  else if is_prim_subtype pty2 pty1
  then Ok pty1
  else Or_error.of_exn (Type_error ("join error", loc))
;;

let meet_prim ~loc pty1 pty2 =
  if is_prim_subtype pty1 pty2
  then Ok pty1
  else if is_prim_subtype pty2 pty1
  then Ok pty2
  else Or_error.of_exn (Type_error ("meet error", loc))
;;

let rec join_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = join_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_var name1, Btyv_var name2 when String.(name1 = name2) -> Ok (Btyv_var name1)
  | Btyv_sum (tyv11, tyv12), Btyv_sum (tyv21, tyv22) ->
    let%bind tyv1' = join_type ~loc tyv11 tyv21 in
    let%bind tyv2' = join_type ~loc tyv12 tyv22 in
    Ok (Btyv_prod (tyv1', tyv2'))
  | Btyv_prod (tyv11, tyv12), Btyv_prod (tyv21, tyv22) ->
    let%bind tyv1' = join_type ~loc tyv11 tyv21 in
    let%bind tyv2' = join_type ~loc tyv12 tyv22 in
    Ok (Btyv_prod (tyv1', tyv2'))
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2' = join_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2'
    then Ok (Btyv_dist tyv1')
    else Or_error.of_exn (Type_error ("join error", loc))
  | _ -> Or_error.of_exn (Type_error ("join error", loc))

and meet_type ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind pty = meet_prim ~loc pty1 pty2 in
    Ok (Btyv_prim pty)
  | Btyv_var name1, Btyv_var name2 when String.(name1 = name2) -> Ok (Btyv_var name1)
  | Btyv_sum (tyv11, tyv12), Btyv_sum (tyv21, tyv22) ->
    let%bind tyv1' = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2' = meet_type ~loc tyv12 tyv22 in
    Ok (Btyv_prod (tyv1', tyv2'))
  | Btyv_prod (tyv11, tyv12), Btyv_prod (tyv21, tyv22) ->
    let%bind tyv1' = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2' = meet_type ~loc tyv12 tyv22 in
    Ok (Btyv_prod (tyv1', tyv2'))
  | Btyv_arrow (tyv11, tyv12), Btyv_arrow (tyv21, tyv22) ->
    let%bind tyv1' = join_type ~loc tyv11 tyv21 in
    let%bind tyv2' = meet_type ~loc tyv12 tyv22 in
    Ok (Btyv_arrow (tyv1', tyv2'))
  | Btyv_dist tyv1', Btyv_dist tyv2' ->
    if equal_base_tyv tyv1' tyv2'
    then Ok (Btyv_dist tyv1')
    else Or_error.of_exn (Type_error ("meet error", loc))
  | _ -> Or_error.of_exn (Type_error ("meet error", loc))
;;

let is_prim_numeric = function
  | Pty_ureal | Pty_preal | Pty_real | Pty_fnat _ | Pty_nat -> true
  | _ -> false
;;

let tycheck_bop_prim bop pty1 pty2 =
  match bop.txt, pty1, pty2 with
  | Bop_add, Pty_ureal, Pty_ureal | Bop_add, Pty_ureal, Pty_preal -> Ok Pty_preal
  | Bop_add, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_add, Pty_preal, Pty_ureal | Bop_add, Pty_preal, Pty_preal -> Ok Pty_preal
  | Bop_add, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_add, Pty_real, Pty_ureal
  | Bop_add, Pty_real, Pty_preal
  | Bop_add, Pty_real, Pty_real -> Ok Pty_real
  | Bop_add, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n + m))
  | Bop_add, Pty_fnat _, Pty_nat -> Ok Pty_nat
  | Bop_add, Pty_nat, Pty_fnat _ | Bop_add, Pty_nat, Pty_nat -> Ok Pty_nat
  | Bop_sub, Pty_ureal, Pty_ureal
  | Bop_sub, Pty_ureal, Pty_preal
  | Bop_sub, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_sub, Pty_preal, Pty_ureal
  | Bop_sub, Pty_preal, Pty_preal
  | Bop_sub, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_sub, Pty_real, Pty_ureal
  | Bop_sub, Pty_real, Pty_preal
  | Bop_sub, Pty_real, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_ureal, Pty_ureal -> Ok Pty_ureal
  | Bop_mul, Pty_ureal, Pty_preal -> Ok Pty_preal
  | Bop_mul, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_preal, Pty_ureal | Bop_mul, Pty_preal, Pty_preal -> Ok Pty_preal
  | Bop_mul, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_real, Pty_ureal
  | Bop_mul, Pty_real, Pty_preal
  | Bop_mul, Pty_real, Pty_real -> Ok Pty_real
  | Bop_mul, Pty_fnat n, Pty_fnat m -> Ok (Pty_fnat (n * m))
  | Bop_mul, Pty_fnat _, Pty_nat -> Ok Pty_nat
  | Bop_mul, Pty_nat, Pty_fnat _ | Bop_mul, Pty_nat, Pty_nat -> Ok Pty_nat
  | Bop_div, Pty_ureal, Pty_ureal | Bop_div, Pty_ureal, Pty_preal -> Ok Pty_preal
  | Bop_div, Pty_ureal, Pty_real -> Ok Pty_real
  | Bop_div, Pty_preal, Pty_ureal | Bop_div, Pty_preal, Pty_preal -> Ok Pty_preal
  | Bop_div, Pty_preal, Pty_real -> Ok Pty_real
  | Bop_div, Pty_real, Pty_ureal
  | Bop_div, Pty_real, Pty_preal
  | Bop_div, Pty_real, Pty_real -> Ok Pty_real
  | (Bop_eq, pty1, pty2 | Bop_ne, pty1, pty2)
    when is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1 -> Ok Pty_bool
  | (Bop_lt, pty1, pty2 | Bop_le, pty1, pty2 | Bop_gt, pty1, pty2 | Bop_ge, pty1, pty2)
    when is_prim_numeric pty1 && (is_prim_subtype pty1 pty2 || is_prim_subtype pty2 pty1)
    -> Ok Pty_bool
  | Bop_and, Pty_bool, Pty_bool | Bop_or, Pty_bool, Pty_bool -> Ok Pty_bool
  | _ ->
    Or_error.of_exn
      (Type_error ("mismatched operand types : " ^ show_binop bop.txt, bop.loc))
;;

let tycheck_bop bop arg1 arg2 =
  match arg1, arg2 with
  | Btyv_prim pty1, Btyv_prim pty2 ->
    let%bind res = tycheck_bop_prim bop pty1 pty2 in
    Ok (Btyv_prim res)
  | _ ->
    Or_error.of_exn
      (Type_error ("mismatched operand types : " ^ show_binop bop.txt, bop.loc))
;;

let rec tycheck_exp_impl globals locals exp =
  match exp.exp_desc with
  | E_var var_name ->
    (match lookup_pures globals locals ~key:var_name.txt with
     | Some tyv -> Ok tyv
     | None ->
       if String.equal var_name.txt "LogSumExp"
       then Or_error.of_exn (Type_error (var_name.txt, exp.exp_loc))
       else
         Or_error.of_exn (Type_error ("undefined variable " ^ var_name.txt, exp.exp_loc)))
  | E_triv -> Ok (Btyv_prim Pty_unit)
  | E_bool _ -> Ok (Btyv_prim Pty_bool)
  | E_cond (exp0, exp1, exp2) ->
    let%bind tyv0 = tycheck_exp globals locals exp0 in
    if is_subtype tyv0 (Btyv_prim Pty_bool)
    then (
      let%bind tyv1 = tycheck_exp globals locals exp1 in
      let%bind tyv2 = tycheck_exp globals locals exp2 in
      join_type ~loc:exp.exp_loc tyv1 tyv2)
    else Or_error.of_exn (Type_error ("non-boolean condition type", exp0.exp_loc))
  | E_real r ->
    if Float.(r >= 0. && r <= 1.)
    then Ok (Btyv_prim Pty_ureal)
    else if Float.(r >= 0.)
    then Ok (Btyv_prim Pty_preal)
    else Ok (Btyv_prim Pty_real)
  | E_nat n ->
    if n >= 0
    then Ok (Btyv_prim (Pty_fnat (n + 1)))
    else Or_error.of_exn (Type_error ("negative integers", exp.exp_loc))
  | E_binop (bop, exp1, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind tyv2 = tycheck_exp globals locals exp2 in
    tycheck_bop bop tyv1 tyv2
  | E_abs (arg_name, arg_type, body) ->
    let arg_tyv = erase_loc_base_ty arg_type in
    let%bind ret_type =
      tycheck_exp globals (Map.set locals ~key:arg_name.txt ~data:arg_tyv) body
    in
    Ok (Btyv_arrow (arg_tyv, ret_type))
  | E_app (exp1, exp2) ->
    let%bind rand_type = tycheck_exp globals locals exp2 in
    let%bind rator =
      match tycheck_exp globals locals exp1 with
      | Error exn ->
        (match Error.to_exn exn with
         | Type_error (msg, _) ->
           if String.equal msg "LogSumExp"
           then (
             match rand_type with
             | Btyv_arrow (a, Btyv_prim Pty_real) ->
               Ok
                 (Btyv_arrow
                    (rand_type, Btyv_arrow (a, Btyv_arrow (a, Btyv_prim Pty_real))))
             | _ ->
               Or_error.of_exn
                 (Type_error
                    ("Argument of of LogSumExp should be function type", exp.exp_loc)))
           else Error exn
         | _ -> Error exn)
      | Ok rator -> Ok rator
    in
    (match rator with
     | Btyv_arrow (tyv11, tyv12) ->
       if is_subtype rand_type tyv11
       then Ok tyv12
       else (
         Format.printf "%a@\n" print_base_tyv rand_type;
         Format.printf "%a@\n" print_base_tyv tyv11;
         Or_error.of_exn (Type_error ("[Typing] mismatched argument types", exp2.exp_loc)))
     | _ -> Or_error.of_exn (Type_error ("non-arrow function type", exp.exp_loc)))
  | E_let (exp1, var_name, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind locals' =
      Or_error.try_with (fun () -> Map.add_exn locals ~key:var_name.txt ~data:tyv1)
    in
    tycheck_exp globals locals' exp2
  | E_dist dist ->
    let%bind tyv = tycheck_dist ~loc:exp.exp_loc globals locals dist in
    Ok (Btyv_dist tyv)
  | E_pair (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind tyv2 = tycheck_exp globals locals exp2 in
    Ok (Btyv_prod (tyv1, tyv2))
  | E_field (exp0, field) ->
    let%bind tyv0 = tycheck_exp globals locals exp0 in
    (match tyv0 with
     | Btyv_prod (tyv1, tyv2) ->
       if field = 0
       then Ok tyv1
       else if field = 1
       then Ok tyv2
       else Or_error.of_exn (Type_error ("invalid field", exp.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("non-projectable value", exp0.exp_loc)))
  | E_inf -> Ok (Btyv_prim Pty_real)
  | E_ninf -> Ok (Btyv_prim Pty_real)
  | E_case (_, _, _, _, _) -> failwith "TODO: not implemented"
  | E_inl _ -> failwith "TODO: not implemented"
  | E_inr _ -> failwith "TODO: not implemented"
  | E_logPr (dist, v) ->
    let%bind v_type = tycheck_exp globals locals v in
    let%bind dist_type = tycheck_exp globals locals dist in
    (match dist_type with
     | Btyv_dist bty ->
       if is_subtype v_type bty
       then Ok (Btyv_prim Pty_real)
       else Or_error.of_exn (Type_error ("mismatched types", dist.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | E_logML m ->
    let%bind _ = tycheck_cmd false globals locals m in
    Ok (Btyv_prim Pty_real)

and tycheck_dist ~loc globals locals dist =
  let lift tars goal curs =
    let tcs = List.zip_exn tars curs in
    let%bind res =
      List.fold_result tcs ~init:None ~f:(fun acc (tar, cur) ->
        match cur with
        | Btyv_prim pty when is_prim_subtype pty tar ->
          (match acc with
           | None -> Ok (Some None)
           | Some None -> Ok (Some None)
           | _ -> Or_error.of_exn (Type_error ("mixed tensors and scalars", loc)))
        | _ -> Or_error.of_exn (Type_error ("mismatched parameter types", loc)))
    in
    let res = Option.value_exn res in
    match res with
    | None -> Ok (Btyv_prim goal)
    | _ -> failwith "TODO"
  in
  match dist with
  | D_ber exp ->
    let%bind tyv = tycheck_exp globals locals exp in
    lift [ Pty_ureal ] Pty_bool [ tyv ]
  | D_unif -> Ok (Btyv_prim Pty_ureal)
  | D_beta (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind tyv2 = tycheck_exp globals locals exp2 in
    lift [ Pty_preal; Pty_preal ] Pty_ureal [ tyv1; tyv2 ]
  | D_gamma (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind tyv2 = tycheck_exp globals locals exp2 in
    lift [ Pty_preal; Pty_preal ] Pty_preal [ tyv1; tyv2 ]
  | D_normal (exp1, exp2) ->
    let%bind tyv1 = tycheck_exp globals locals exp1 in
    let%bind tyv2 = tycheck_exp globals locals exp2 in
    lift [ Pty_real; Pty_preal ] Pty_real [ tyv1; tyv2 ]
  | D_cat exps ->
    let n = List.length exps in
    let%bind () =
      List.fold_result exps ~init:() ~f:(fun () exp ->
        let%bind tyv = tycheck_exp globals locals exp in
        if is_subtype tyv (Btyv_prim Pty_preal)
        then Ok ()
        else Or_error.of_exn (Type_error ("mismatched parameter types", loc)))
    in
    Ok (Btyv_prim (Pty_fnat n))
  | D_bin (n, exp) ->
    let%bind tyv = tycheck_exp globals locals exp in
    lift [ Pty_ureal ] (Pty_fnat n) [ tyv ]
  | D_geo exp ->
    let%bind tyv = tycheck_exp globals locals exp in
    lift [ Pty_ureal ] Pty_nat [ tyv ]
  | D_pois exp ->
    let%bind tyv = tycheck_exp globals locals exp in
    lift [ Pty_preal ] Pty_nat [ tyv ]

and tycheck_trm_impl verbose globals locals trm =
  match trm.trm_desc with
  | T_ret exp ->
    let%bind exp_type = tycheck_exp globals locals exp in
    Ok exp_type
  | T_sample dist ->
    let%bind exp_type = tycheck_exp globals locals dist in
    (match exp_type with
     | Btyv_dist tyv -> Ok tyv
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | T_branch (e, tcmd, fcmd) ->
    let%bind e_type = tycheck_exp globals locals e in
    (match e_type with
     | Btyv_prim Pty_bool ->
       let%bind t_btyv = tycheck_cmd verbose globals locals tcmd in
       let%bind f_btyv = tycheck_cmd verbose globals locals fcmd in
       let%bind btyv = join_type ~loc:trm.trm_loc t_btyv f_btyv in
       Ok btyv
     | _ -> Or_error.of_exn (Type_error ("non-boolean condition type", e.exp_loc)))
  | T_observe (dist, obs) ->
    let%bind obs_type = tycheck_exp globals locals obs in
    let%bind dist_type = tycheck_exp globals locals dist in
    (match dist_type with
     | Btyv_dist bty ->
       if is_subtype obs_type bty
       then Ok (Btyv_prim Pty_unit)
       else Or_error.of_exn (Type_error ("mismatched types", dist.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | T_call (proc_name, args) ->
    (match Map.find globals.top_proc_sigvs proc_name.txt with
     | None ->
       Or_error.of_exn (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
     | Some psigv ->
       if List.length psigv.psigv_arg_tys <> List.length args
       then Or_error.of_exn (Type_error ("mismatched arity", trm.trm_loc))
       else (
         let%bind arg_tys =
           List.fold_result (List.rev args) ~init:[] ~f:(fun acc arg ->
             let%bind arg_ty = tycheck_exp globals locals arg in
             Ok (arg_ty :: acc))
         in
         if not
              (List.for_all2_exn arg_tys psigv.psigv_arg_tys ~f:(fun ty (_, ty') ->
                 is_subtype ty ty'))
         then Or_error.of_exn (Type_error ("mismatched argument types", trm.trm_loc))
         else Ok psigv.psigv_ret_ty))
  | T_factor exp ->
    let%bind btyv = tycheck_exp globals locals exp in
    if is_subtype btyv (Btyv_prim Pty_real)
    then Ok (Btyv_prim Pty_unit)
    else Or_error.of_exn (Type_error ("mismatched types", exp.exp_loc))
  | T_choose (lb, ub) ->
    let%bind btyv_lb = tycheck_exp globals locals lb in
    let%bind btyv_ub = tycheck_exp globals locals ub in
    if is_subtype btyv_lb (Btyv_prim Pty_nat) && is_subtype btyv_ub (Btyv_prim Pty_nat)
    then Ok (Btyv_prim Pty_nat)
    else Or_error.of_exn (Type_error ("mismatched types", trm.trm_loc))
  | _ -> Or_error.of_exn (Type_error ("not implemented", trm.trm_loc))

and tycheck_cmd_impl verbose globals locals cmd =
  match cmd.cmd_desc with
  | M_trm trm -> tycheck_trm verbose globals locals trm
  | M_seq (trm, cmd) ->
    let%bind _ = tycheck_trm verbose globals locals trm in
    let%bind cmd_type = tycheck_cmd verbose globals locals cmd in
    Ok cmd_type
  | M_bnd (var, trm, cmd) ->
    let%bind trm_type = tycheck_trm verbose globals locals trm in
    let locals' = Map.add_exn locals ~key:var.txt ~data:trm_type in
    let%bind cmd_type = tycheck_cmd verbose globals locals' cmd in
    Ok cmd_type

and tycheck_exp globals locals exp = tycheck_exp_impl globals locals exp

and tycheck_trm verbose globals locals trm =
  if verbose then Format.printf "tycheck_trm %a\n" print_trm trm;
  let%bind bty = tycheck_trm_impl verbose globals locals trm in
  if verbose
  then Format.printf "%a |- %a : %a\n" print_ctx locals print_trm trm print_base_tyv bty;
  Ok bty

and tycheck_cmd verbose globals locals cmd =
  (* let () = print_endline "tycheck_cmd" in  *)
  (* let () = print_endline (show_cmd cmd) in *)
  let%bind bty = tycheck_cmd_impl verbose globals locals cmd in
  (* let () = print_endline (show_base_tyv bty) in *)
  (* if verbose
     then (
     match cmd.cmd_desc with
     | M_trm _ ->
     Format.printf
     "%a |- %a : %a\n"
     print_ctx ctx
     print_cmd cmd
     print_base_tyv bty
     | _ -> ()); *)
  Ok bty
;;

let tycheck_proc verbose globals proc =
  let psigv = erase_loc_proc_sig proc.proc_sig in
  let%bind locals = String.Map.of_alist_or_error psigv.psigv_arg_tys in
  let%bind bty = tycheck_cmd verbose globals locals proc.proc_body in
  if not (is_subtype bty psigv.psigv_ret_ty)
  then Or_error.of_exn (Type_error ("mismatched signature types", proc.proc_loc))
  else Ok bty
;;

let tycheck_prog verbose prog =
  let%bind top_external_pure_sigvs = collect_external_pures prog in
  let%bind top_proc_sigvs = collect_proc_sigs prog in
  let%bind top_pure_sigvs = collect_pure_sigs prog in
  let global = { top_external_pure_sigvs; top_proc_sigvs; top_pure_sigvs } in
  List.fold_result prog ~init:() ~f:(fun _ top ->
    match top with
    | Top_proc (proc_name, proc) ->
      (match tycheck_proc verbose global proc with
       | Ok _ ->
         print_endline ("proc " ^ proc_name.txt ^ " pass check");
         Ok ()
       | Error exn ->
         print_endline ("proc " ^ proc_name.txt ^ " failed check");
         Error exn)
    | Top_pure (name, ret_ty, body) ->
      (match tycheck_exp global String.Map.empty body with
       | Ok bty ->
         if is_subtype bty (erase_loc_base_ty ret_ty)
         then (
           print_endline ("pure " ^ name.txt ^ " pass check");
           Ok ())
         else Or_error.of_exn (Type_error ("mismatched signature types", ret_ty.bty_loc))
       | Error exn ->
         print_endline ("pure " ^ name.txt ^ " failed check");
         Error exn)
    | _ -> Ok ())
;;
