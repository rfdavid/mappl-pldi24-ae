open Core
open AbstractSyntaxTree
open Or_error.Let_syntax
open Type
open Identifier

let index = ref 0
let reset_level_variable () = index := 0

let gen_new_index () =
  let c = !index in
  (* Increased for compilation time benchmarks *)
  if c > 1000 then failwith "[gen_new_index] too many vars";
  incr index;
  c
;;

let gen_lambda_name () = "lambda_" ^ string_of_int (gen_new_index ())
let gen_logML_name () = "logML_" ^ string_of_int (gen_new_index ())

let closure_conversion locals lambda ty =
  let fresh_name = gen_lambda_name () in
  let fresh_var = make_id fresh_name in
  let used_locals = locals |> Map.filter_keys ~f:(apperas_exp lambda) in
  let lambda, lambda_ty =
    used_locals
    |> Map.fold ~init:(lambda, ty) ~f:(fun ~key ~data (lambda, ty) ->
      let btyv = to_base_ty data in
      let free_var = make_id @@ key in
      let lambda' = mknoloc_exp @@ E_abs (free_var, btyv, lambda) in
      let ty' = mknoloc_bty @@ Bty_arrow (btyv, ty) in
      lambda', ty')
  in
  let closure =
    used_locals
    |> Map.fold_right
         ~init:(mknoloc_exp @@ E_var fresh_var)
         ~f:(fun ~key ~data:_ closure ->
           let free_var = make_id @@ key in
           let closure' = mknoloc_exp @@ E_app (closure, mknoloc_exp @@ E_var free_var) in
           closure')
  in
  Ok (Top_pure (fresh_var, lambda_ty, lambda), closure)
;;

let closure_conversion_top_func
  ?(top_name = None)
  locals
  arg_names
  arg_tys
  body
  psig_ret_ty
  =
  let fresh_name = top_name |> Option.value_or_thunk ~default:gen_lambda_name in
  let fresh_var = make_id fresh_name in
  let used_locals =
    locals
    |> Map.filter_keys ~f:(fun vname ->
      (not (List.exists arg_names ~f:(fun arg_name -> String.equal arg_name.txt vname)))
      && apperas_exp body vname)
    |> Map.to_alist
    |> List.map ~f:(fun (name, ty) -> make_id name, to_base_ty ty)
  in
  let psig_arg_tys = used_locals @ List.zip_exn arg_names arg_tys in
  let func_sig = { psig_arg_tys; psig_ret_ty } in
  let args = used_locals |> List.map ~f:(fun (key, _) -> mknoloc_exp @@ E_var key) in
  let closure = mknoloc_exp @@ E_call (fresh_var, args) in
  let func_def = { func_sig; func_body = body; func_loc = Location.none } in
  Ok (Top_func (fresh_var, func_def), closure)
;;

let rec hoist_exp ?(top_name = None) globals locals exp =
  (* Format.printf "[hoist_exp]@\n%a@;<1 4>|-@;<1 8>%a@\n" Typing.print_ctx locals print_exp exp; *)
  match exp.exp_desc with
  | E_abs (arg_name, arg_type, body) ->
    let arg_tyv = erase_loc_base_ty arg_type in
    let locals' =
      locals |> Map.add_exn ~key:arg_name.txt ~data:(erase_loc_base_ty arg_type)
    in
    let%bind ty = Typing.tycheck_exp globals locals' body in
    let locals' = locals |> Map.add_exn ~key:arg_name.txt ~data:arg_tyv in
    let%bind body', lifted = hoist_exp globals locals' body in
    (* let lambda = mknoloc_exp @@ E_abs (arg_name, arg_type, body') in *)
    let%bind top_func, closure =
      closure_conversion_top_func
        ~top_name
        locals
        [ arg_name ]
        [ arg_type ]
        body'
        (to_base_ty ty)
    in
    Ok (closure, top_func :: lifted)
  | E_let (exp1, var_name, exp2) ->
    let%bind tyv1 = Typing.tycheck_exp globals locals exp1 in
    let%bind exp1', lifted1 = hoist_exp globals locals exp1 in
    let locals' = locals |> Map.add_exn ~key:var_name.txt ~data:tyv1 in
    let%bind exp2', lifted2 = hoist_exp globals locals' exp2 in
    Ok (mknoloc_exp @@ E_let (exp1', var_name, exp2'), lifted1 @ lifted2)
  | E_cond (exp0, exp1, exp2) ->
    let%bind exp0', lifted0 = hoist_exp globals locals exp0 in
    let%bind exp1', lifted1 = hoist_exp globals locals exp1 in
    let%bind exp2', lifted2 = hoist_exp globals locals exp2 in
    Ok (mknoloc_exp @@ E_cond (exp0', exp1', exp2'), lifted0 @ lifted1 @ lifted2)
  | E_binop (bop, exp1, exp2) ->
    let%bind exp1', lifted1 = hoist_exp globals locals exp1 in
    let%bind exp2', lifted2 = hoist_exp globals locals exp2 in
    Ok (mknoloc_exp @@ E_binop (bop, exp1', exp2'), lifted1 @ lifted2)
  | E_logML _ ->
    let logMLname = top_name |> Option.value_or_thunk ~default:gen_logML_name in
    let%bind top_func, closure =
      closure_conversion_top_func
        ~top_name:(Some logMLname)
        locals
        []
        []
        exp
        (to_base_ty @@ Btyv_prim Pty_real)
    in
    Ok (closure, [ top_func ])
  | _ -> Ok (exp, [])
;;

let hoist_top globals top =
  (* Format.printf "%a@." print_top_level top; *)
  match top with
  | Top_pure (name, ty, def) ->
    let res =
      let%bind _, lifted =
        hoist_exp ~top_name:(Some name.txt) globals String.Map.empty def
      in
      (* Format.printf "hoist_top@\n%a@\n" print_exp def'; *)
      match def.exp_desc, ty.bty_desc with
      | E_abs _, Bty_arrow _ -> Ok lifted
      | _ -> failwith "[hoist_top]TODO"
    in
    Some res
  | Top_external_pure _ -> Some (Ok [ top ])
  | Top_func (func_name, { func_body; func_sig; func_loc }) ->
    let res =
      let psigv = Typing.erase_loc_proc_sig func_sig in
      let%bind locals = String.Map.of_alist_or_error psigv.psigv_arg_tys in
      let%bind closure, lifted =
        hoist_exp ~top_name:(Some func_name.txt) globals locals func_body
      in
      let func' = { func_body = closure; func_sig; func_loc } in
      Ok (Top_func (func_name, func') :: lifted)
    in
    Some res
  | _ -> failwith "[hoist_top]TODO"
;;

let hoist prog =
  let%bind top_external_pure_sigvs = Typing.collect_external_pures prog in
  let%bind top_proc_sigvs = Typing.collect_proc_sigs prog in
  let%bind top_pure_sigvs = Typing.collect_pure_sigs prog in
  let global = { Type.top_external_pure_sigvs; top_proc_sigvs; top_pure_sigvs } in
  let hoisted_results = List.filter_map prog ~f:(hoist_top global) in
  let succed, failed = List.partition_result hoisted_results in
  if List.length failed > 0
  then Error (List.hd_exn failed)
  else (
    let hosited = List.concat succed in
    Ok hosited)
;;
