open Core
open Or_error.Let_syntax
open Identifier
open AbstractSyntaxTree
open Type
module IType = InformationType
module ITyping = InformationTyping

exception VE_Error of string * Location.t

let index = ref 0
let reset_level_variable () = index := 0

let gen_new_index () =
  let c = !index in
  if c > 100 then failwith "[gen_new_index] too many vars";
  incr index;
  c
;;

let gen_ret_name () = "ret_" ^ string_of_int (gen_new_index ())

let halt_transformed btyv =
  let id = make_id @@ "_" in
  let body = mknoloc_exp @@ E_real 0.0 in
  let k = mknoloc_exp @@ E_abs (id, to_base_ty btyv, body) in
  k
;;

let map_merge map1 map2 =
  Map.fold map1 ~init:map2 ~f:(fun ~key ~data m -> Map.add_exn m ~key ~data)
;;

let exp_app_args rator args =
  List.fold args ~init:rator ~f:(fun r arg -> mknoloc_exp @@ E_app (r, arg))
;;

let rec cmd_to_terms k cmd =
  match cmd.cmd_desc with
  | M_trm trm -> [ gen_ret_name (), trm ]
  | M_seq (trm, cmd) -> ("_", trm) :: cmd_to_terms k cmd
  | M_bnd (id, trm, cmd) -> (id.txt, trm) :: cmd_to_terms k cmd
;;

let rec concat_cmd ({ cmd_desc; cmd_loc; _ } as lhs) rhs =
  (* Format.printf "[concat_cmd]\nlhs:\t%a\nrhs:\t%a\n" print_cmd lhs print_cmd rhs; *)
  (* if (equal_cmd lhs (M_trm (T_ret (E_triv)))) then  *)
  if equal_cmd lhs (mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_ret (mknoloc_exp @@ E_triv)))
  then rhs
  else (
    match cmd_desc with
    | M_trm trm -> mknoloc_cmd @@ M_seq (trm, rhs)
    | M_seq (trm, cmd) ->
      { cmd_loc; cmd_desc = M_seq (trm, concat_cmd cmd rhs); cmd_type = None }
    | M_bnd (id, trm, cmd) ->
      { cmd_loc; cmd_desc = M_bnd (id, trm, concat_cmd cmd rhs); cmd_type = None })
;;

let rec equal_fmtnel_id_return k { cmd_desc; cmd_loc; _ } =
  match cmd_desc with
  | M_trm trm ->
    let name = make_id @@ gen_ret_name () in
    { cmd_loc
    ; cmd_desc =
        M_bnd
          ( name
          , trm
          , mknoloc_cmd
            @@ M_trm
                 (mknoloc_trm
                  @@ T_factor (mknoloc_exp @@ E_app (k, mknoloc_exp @@ E_var name))) )
    ; cmd_type = None
    }
  | M_seq (trm, cmd) ->
    { cmd_loc; cmd_desc = M_seq (trm, equal_fmtnel_id_return k cmd); cmd_type = None }
  | M_bnd (id, trm, cmd) ->
    { cmd_loc; cmd_desc = M_bnd (id, trm, equal_fmtnel_id_return k cmd); cmd_type = None }
;;

let try_elim_disc
  verbose
  (gloabals : IType.global_context)
  (info_locals : IType.context)
  locals
  var
  ({ trm_desc; trm_loc; _ } as trm)
  =
  if verbose then Format.printf "[try_elim_disc]@\n%a@\n" print_trm trm;
  match trm_desc with
  | T_sample d ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind bty =
      Typing.tycheck_exp (ITyping.erase_label_global_context gloabals) combined_locals d
    in
    (match bty with
     | Btyv_dist bty' ->
       if Typing.is_subtype bty' (Btyv_prim Pty_bool)
       then
         Ok
           { trm_loc
           ; trm_desc = T_factor (mknoloc_exp @@ E_logPr (d, var))
           ; trm_type = None
           }
       else
         (* TODO support distributions type other thant Dist of Bool*)
         Or_error.of_exn @@ VE_Error ("not discrete", trm_loc)
     | _ -> Or_error.of_exn @@ VE_Error ("not a distribution", trm_loc))
  | _ -> Or_error.of_exn @@ VE_Error ("not a sample", trm_loc)
;;

let elim_disc verbose gloabals info_locals locals var trm =
  let result = try_elim_disc verbose gloabals info_locals locals var trm in
  Option.value (Or_error.ok result) ~default:trm
;;

let rec split_impl
  verbose
  (info_globals : IType.global_context)
  (locals : IType.context)
  ({ cmd_desc; cmd_loc; _ } as cmd)
  =
  if verbose
  then
    Format.printf
      "[split_impl]\n%a\n[split_impl]\t%a\n"
      ITyping.print_ctx
      locals
      print_cmd
      cmd;
  match cmd_desc with
  | M_trm trm ->
    let%bind ty = ITyping.type_check_trm verbose info_globals locals trm in
    if%bind ITyping.outter_is_L ty
    then (
      let mH = mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_ret (mknoloc_exp @@ E_triv)) in
      let mL = { cmd_desc; cmd_loc; cmd_type = None } in
      Ok (mH, mL))
    else (
      let mH = { cmd_desc; cmd_loc; cmd_type = None } in
      let mL = mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_ret (mknoloc_exp @@ E_triv)) in
      Ok (mH, mL))
  | M_seq (t, m) ->
    let%bind ty = ITyping.type_check_trm verbose info_globals locals t in
    let%bind mH, mL = split_impl verbose info_globals locals m in
    if%bind ITyping.outter_is_L ty
    then Ok (mH, { cmd_loc; cmd_desc = M_seq (t, mL); cmd_type = None })
    else Ok ({ cmd_loc; cmd_desc = M_seq (t, mH); cmd_type = None }, mL)
  | M_bnd (id, t, m) ->
    let%bind ty = ITyping.type_check_trm verbose info_globals locals t in
    let locals' = locals |> Map.add_exn ~key:id.txt ~data:ty in
    let%bind mH, mL = split_impl verbose info_globals locals' m in
    if%bind ITyping.is_all_L_ltyv ty
    then
      (* if is_L && (not (apperas_cmd mL id.txt)) then *)
      Ok (mH, { cmd_loc; cmd_desc = M_bnd (id, t, mL); cmd_type = None })
    else Ok ({ cmd_loc; cmd_desc = M_bnd (id, t, mH); cmd_type = None }, mL)
;;

let split
  verbose
  (info_globals : IType.global_context)
  (info_locals : IType.context)
  (locals : Type.context)
  ~(key : string)
  ~(btyv : base_tyv)
  cmd
  =
  if verbose
  then (
    Format.printf "[split] %s\n" key;
    Format.printf "[split]@.%a@." ITyping.print_ctx info_locals;
    Format.printf "[split] %a\n" print_cmd cmd);
  let lty = ITyping.outterH btyv in
  let info_locals' =
    info_locals
    |> Map.add_exn ~key ~data:lty
    |> map_merge (ITyping.annotate_L_context locals)
  in
  (* let%bind cmd',constraints = ITyping.tycheck_cmd false info_globals info_locals' cmd in *)
  (* Format.printf "[split todo]@.%a@." ITyping.print_typed_cmd cmd'; *)
  (* let%bind s = Solver.solve false constraints in *)
  (* let cmd'' = ITyping.subst_level_cmd s cmd' in *)
  (* Format.printf "[split todo]@.%a@." ITyping.print_typed_cmd cmd''; *)
  let%bind mH, mL =
    split_impl
      verbose
      (info_globals : IType.global_context)
      (info_locals' : IType.context)
      cmd
  in
  if verbose
  then (
    Format.printf "[split resutl mH]@.%a@." print_cmd mH;
    Format.printf "[split resutl mL]@.%a@." print_cmd mL);
  Ok (mH, mL)
;;

let trm_to_exp
  verbose
  (info_globals : IType.global_context)
  (info_locals : IType.context)
  { trm_desc; _ }
  =
  if verbose
  then (
    Format.printf "[trm_to_exp]@\n%a@\n" ITyping.print_global_context info_globals;
    Format.printf "[trm_to_exp]@\n%a@\n" ITyping.print_ctx info_locals);
  match trm_desc with
  | T_ret _ -> mknoloc_exp @@ E_real 0.0
  | T_factor e -> e
  | _ -> failwith "trm_to_exp"
;;

let rec elim_D_intro
  verbose
  (gloabals : IType.global_context)
  (info_locals : IType.context)
  locals
  ({ cmd_desc; cmd_loc; _ } as cmd)
  =
  let cmd_type = None in
  if verbose
  then
    Format.printf
      "[elim_D_intro]\t%a\n\t%a|-%a\n"
      ITyping.print_ctx
      info_locals
      Typing.print_ctx
      locals
      print_cmd
      cmd;
  match cmd_desc with
  | M_trm _ -> Ok ({ cmd_desc; cmd_loc; cmd_type }, [])
  | M_seq ({ trm_desc = T_sample _; _ }, _) ->
    Or_error.of_exn @@ VE_Error ("anonymous random variable", cmd_loc)
  | M_seq ({ trm_desc = T_observe (d, v); _ }, cmd) ->
    let%bind cmd', todo = elim_D_intro verbose gloabals info_locals locals cmd in
    let trm' = mknoloc_trm @@ T_factor (mknoloc_exp @@ E_logPr (d, v)) in
    Ok ({ cmd_loc; cmd_desc = M_seq (trm', cmd'); cmd_type }, todo)
  | M_seq (trm, cmd) ->
    let%bind cmd', todo = elim_D_intro verbose gloabals info_locals locals cmd in
    Ok ({ cmd_loc; cmd_desc = M_seq (trm, cmd'); cmd_type }, todo)
  | M_bnd (id, trm, cmd) ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context gloabals)
        combined_locals
        trm
    in
    let locals' = Map.add_exn locals ~key:id.txt ~data:btyv in
    let%bind cmd', todos = elim_D_intro verbose gloabals info_locals locals' cmd in
    (match
       try_elim_disc verbose gloabals info_locals locals' (mknoloc_exp @@ E_var id) trm
     with
     | Ok trm' ->
       Ok ({ cmd_loc; cmd_desc = M_seq (trm', cmd'); cmd_type }, (id, btyv) :: todos)
     | _ -> Ok ({ cmd_loc; cmd_desc = M_bnd (id, trm, cmd'); cmd_type }, todos))
;;

let rec elim_control_flow
  verbose
  (info_globals : IType.global_context)
  (info_locals : IType.context)
  (locals : Type.context)
  (todo : (Identifier.variable_id * Type.base_tyv) list)
  ({ cmd_desc; cmd_loc; _ } as cmd)
  =
  if verbose
  then
    Format.printf
      "[elim_control_flow]\n%a\n\t%a\n"
      ITyping.print_ctx
      info_locals
      print_cmd
      cmd;
  match cmd_desc with
  | M_trm _ -> Ok cmd
  | M_seq (({ trm_desc = T_factor _; _ } as trm), cmd) ->
    let%bind cmd' = elim_control_flow verbose info_globals info_locals locals todo cmd in
    Ok { cmd_loc; cmd_desc = M_seq (trm, cmd'); cmd_type = None }
  | M_seq (({ trm_desc = T_call (name, args); _ } as trm), cmd) ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context info_globals)
        combined_locals
        trm
    in
    let k = halt_transformed btyv in
    let k_trm = mknoloc_trm @@ T_ret k in
    let fresh = gen_ret_name () in
    let k_id = make_id @@ "k_of_" ^ fresh in
    let call_exp =
      exp_app_args (mknoloc_exp @@ E_var name) ((mknoloc_exp @@ E_var k_id) :: args)
    in
    let call_cmd =
      mknoloc_cmd
      @@ M_bnd (k_id, k_trm, mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor call_exp))
    in
    let cmd' = concat_cmd cmd call_cmd in
    elim_control_flow verbose info_globals info_locals locals todo cmd'
  | M_bnd (id, ({ trm_desc = T_branch (e, m_t, m_f); _ } as trm), cmd) ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context info_globals)
        combined_locals
        trm
    in
    let%bind mH, mL =
      split verbose info_globals info_locals locals ~key:id.txt ~btyv cmd
    in
    let k_id = make_id @@ "k_of_" ^ id.txt in
    let%bind eH = elim_cmd verbose info_globals info_locals locals mH in
    let bty = to_base_ty btyv in
    let k = mknoloc_exp @@ E_abs (id, bty, eH) in
    let k_trm = mknoloc_trm @@ T_ret k in
    let%bind k_type =
      ITyping.type_check ~f:ITyping.tycheck_exp verbose info_globals info_locals k
    in
    let info_locals' = info_locals |> Map.add_exn ~key:k_id.txt ~data:k_type in
    let locals' = locals |> Map.add_exn ~key:id.txt ~data:btyv in
    let m_t' = equal_fmtnel_id_return (mknoloc_exp @@ E_var k_id) m_t in
    let%bind e_t = elim_cmd verbose info_globals info_locals' locals' m_t' in
    let m_f' = equal_fmtnel_id_return (mknoloc_exp @@ E_var k_id) m_f in
    let%bind e_f = elim_cmd verbose info_globals info_locals' locals' m_f' in
    let e_if = mknoloc_exp @@ E_cond (e, e_t, e_f) in
    let if_cmd =
      mknoloc_cmd
      @@ M_bnd (k_id, k_trm, mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor e_if))
    in
    Ok (concat_cmd mL if_cmd)
  | M_bnd (id, ({ trm_desc = T_call (name, args); _ } as trm), cmd) ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context info_globals)
        combined_locals
        trm
    in
    let%bind mH, mL =
      split verbose info_globals info_locals locals ~key:id.txt ~btyv cmd
    in
    let locals' = locals |> Map.add_exn ~key:id.txt ~data:btyv in
    let%bind eH = elim_cmd verbose info_globals info_locals locals' mH in
    let k = mknoloc_exp @@ E_abs (id, to_base_ty btyv, eH) in
    let k_trm = mknoloc_trm @@ T_ret k in
    let k_id = make_id @@ "k_of_" ^ id.txt in
    let call_exp =
      exp_app_args (mknoloc_exp @@ E_var name) ((mknoloc_exp @@ E_var k_id) :: args)
    in
    let call_cmd =
      mknoloc_cmd
      @@ M_bnd (k_id, k_trm, mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor call_exp))
    in
    Ok (concat_cmd mL call_cmd)
  | M_bnd (id, { trm_desc = T_ret e; trm_loc; trm_type }, cmd) ->
    let trm = { trm_desc = T_ret e; trm_loc; trm_type } in
    let%bind cmd' = elim_control_flow verbose info_globals info_locals locals todo cmd in
    Ok { cmd_loc; cmd_desc = M_bnd (id, trm, cmd'); cmd_type = None }
  | M_bnd (id, ({ trm_desc = T_sample _; _ } as trm), cmd) ->
    let%bind cmd' = elim_control_flow verbose info_globals info_locals locals todo cmd in
    Ok { cmd_loc; cmd_desc = M_bnd (id, trm, cmd'); cmd_type = None }
  | M_bnd (id, ({ trm_desc = T_factor _; _ } as trm), cmd) ->
    let%bind cmd' = elim_control_flow verbose info_globals info_locals locals todo cmd in
    Ok { cmd_loc; cmd_desc = M_bnd (id, trm, cmd'); cmd_type = None }
  | M_bnd (id, { trm_desc = T_choose (lb, ub); trm_loc; trm_type }, cmd) ->
    let trm = { trm_desc = T_choose (lb, ub); trm_loc; trm_type } in
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context info_globals)
        combined_locals
        trm
    in
    let%bind mH, mL =
      split verbose info_globals info_locals locals ~key:id.txt ~btyv cmd
    in
    if verbose
    then (
      Format.printf "[T_choose mH]%a\n" print_cmd mH;
      Format.printf "[T_choose mL]%a\n" print_cmd mL;
      Format.printf "%a\n" print_base_tyv btyv);
    let locals' = locals |> Map.add_exn ~key:id.txt ~data:btyv in
    let%bind eH = elim_cmd verbose info_globals info_locals locals' mH in
    let k = mknoloc_exp @@ E_abs (id, to_base_ty btyv, eH) in
    let k_id = make_id @@ "k_of_" ^ id.txt in
    let k_trm = mknoloc_trm @@ T_ret k in
    let y_exp =
      mknoloc_exp
      @@ E_app
           ( mknoloc_exp
             @@ E_app
                  ( mknoloc_exp
                    @@ E_app
                         ( mknoloc_exp @@ E_var (make_id "LogSumExp")
                         , mknoloc_exp @@ E_var k_id )
                  , lb )
           , ub )
    in
    let y_cmd = mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor y_exp) in
    let cmd' = concat_cmd mL (mknoloc_cmd @@ M_bnd (k_id, k_trm, y_cmd)) in
    Ok (concat_cmd mL cmd')
  | _ -> failwith "TODO: elim_control_flow"

and elim_D_elim
  verbose
  info_globals
  info_locals
  locals
  (todo : (variable_id * base_tyv) list)
  cmd
  =
  if verbose
  then (
    Format.printf "[elim_D_elim]@.%a@." ITyping.print_ctx info_locals;
    Format.printf "[elim_D_elim]@.%a@." Typing.print_ctx locals;
    Format.printf "[elim_D_elim]@.%a@." print_cmd cmd);
  match todo with
  | [] -> Ok cmd
  | (y, bty_y) :: todo ->
    let todo' = List.map todo ~f:(fun (name, btyv) -> name.txt, btyv) in
    let locals' = map_merge locals (String.Map.of_alist_exn todo') in
    let%bind mH, mL =
      split verbose info_globals info_locals locals ~key:y.txt ~btyv:bty_y cmd
    in
    if verbose
    then (
      Format.printf "[elim_D_elim mH]%a\n" print_cmd mH;
      Format.printf "[elim_D_elim mL]%a\n" print_cmd mL);
    let locals'' = locals' |> Map.add_exn ~key:y.txt ~data:bty_y in
    let%bind eH = elim_cmd verbose info_globals info_locals locals'' mH in
    let k = mknoloc_exp @@ E_abs (y, to_base_ty bty_y, eH) in
    let k_id = make_id @@ "k_of_" ^ y.txt in
    let k_trm = mknoloc_trm @@ T_ret k in
    (* let y_exp = mknoloc_exp @@ E_app (mknoloc_exp @@ E_var (make_id "LogSumExp"), mknoloc_exp @@ E_var k_id) in *)
    let y_exp =
      mknoloc_exp
      @@ E_app
           ( mknoloc_exp
             @@ E_app
                  ( mknoloc_exp
                    @@ E_app
                         ( mknoloc_exp @@ E_var (make_id "LogSumExp")
                         , mknoloc_exp @@ E_var k_id )
                  , mknoloc_exp @@ E_bool false )
           , mknoloc_exp @@ E_bool true )
    in
    let y_cmd = mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor y_exp) in
    let cmd' = concat_cmd mL (mknoloc_cmd @@ M_bnd (k_id, k_trm, y_cmd)) in
    elim_D_elim verbose info_globals info_locals locals todo cmd'

and elim_cmd
  verbose
  (info_globals : IType.global_context)
  (info_locals : IType.context)
  (locals : Type.context)
  cmd
  =
  if verbose
  then (
    Format.printf "[elim_cmd]@.%a@." ITyping.print_ctx info_locals;
    Format.printf "[elim_cmd]@.%a@." Typing.print_ctx locals;
    Format.printf "[elim_cmd]@.%a@." print_cmd cmd);
  let%bind m_no_sample, todo = elim_D_intro verbose info_globals info_locals locals cmd in
  let%bind m_summed =
    elim_D_elim verbose info_globals info_locals locals todo m_no_sample
  in
  let%bind m' = elim_control_flow verbose info_globals info_locals locals todo m_summed in
  if verbose then Format.printf "[elim_cmd reuslt]@.%a@." print_cmd m';
  let%bind e = cmd_to_exp verbose info_globals info_locals locals m' in
  Ok e

and cmd_to_exp
  verbose
  (info_globals : IType.global_context)
  (info_locals : IType.context)
  locals
  ({ cmd_desc; cmd_loc; _ } as cmd)
  =
  if verbose then Format.printf "[cmd_to_exp]@.%a@." print_cmd cmd;
  match cmd_desc with
  | M_trm trm -> Ok (trm_to_exp verbose info_globals info_locals trm)
  | M_seq ({ trm_desc = T_factor e_trm; _ }, cmd) ->
    let%bind e_cmd = cmd_to_exp verbose info_globals info_locals locals cmd in
    Ok { exp_desc = E_binop (Location.mknoloc Bop_add, e_trm, e_cmd); exp_loc = cmd_loc }
  | M_seq ({ trm_desc = T_ret _; _ }, cmd) ->
    let%bind e_cmd = cmd_to_exp verbose info_globals info_locals locals cmd in
    Ok e_cmd
  | M_bnd (id, { trm_desc = T_ret e_trm; _ }, cmd) ->
    (*
       let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
       let%bind btyv = Typing.tycheck_trm verbose (ITyping.erase_label_global_context info_globals) combined_locals trm in
    *)
    let%bind e_cmd = cmd_to_exp verbose info_globals info_locals locals cmd in
    Ok { exp_desc = E_let (e_trm, id, e_cmd); exp_loc = cmd_loc }
  | M_bnd (id, ({ trm_desc = T_sample _; _ } as trm), cmd) ->
    let combined_locals = map_merge locals (ITyping.erase_label_context info_locals) in
    let%bind btyv =
      Typing.tycheck_trm
        verbose
        (ITyping.erase_label_global_context info_globals)
        combined_locals
        trm
    in
    let%bind mH, mL =
      split verbose info_globals info_locals locals ~key:id.txt ~btyv cmd
    in
    let locals' = locals |> Map.add_exn ~key:id.txt ~data:btyv in
    (* TODO locals should also contains bindings created by mL *)
    let%bind eH = elim_cmd verbose info_globals info_locals locals' mH in
    let factor_eH = mknoloc_cmd @@ M_trm (mknoloc_trm @@ T_factor eH) in
    let y_cmd = mknoloc_cmd @@ M_bnd (id, trm, factor_eH) in
    let logML_y_cmd = mknoloc_exp @@ E_logML y_cmd in
    let factor_logML_y_cmd = mknoloc_trm @@ T_factor logML_y_cmd in
    let cmd' = concat_cmd mL (mknoloc_cmd @@ M_trm factor_logML_y_cmd) in
    cmd_to_exp
      verbose
      (info_globals : IType.global_context)
      (info_locals : IType.context)
      locals
      cmd'
  | M_bnd (id, ({ trm_desc = T_factor _; _ } as trm), cmd) ->
    let return_triv = mknoloc_trm @@ T_ret (mknoloc_exp @@ E_triv) in
    let cmd' = mknoloc_cmd @@ M_seq (trm, mknoloc_cmd @@ M_bnd (id, return_triv, cmd)) in
    cmd_to_exp verbose info_globals info_locals locals cmd'
  | _ ->
    Format.printf "[cmd_to_exp]\t%a\n" print_cmd cmd;
    failwith "cmd_to_exp"
;;

let elim_proc_sig (proc_sig : proc_sig) (body : exp) =
  let e =
    List.fold_right proc_sig.psig_arg_tys ~init:body ~f:(fun (var, ty) e ->
      mknoloc_exp @@ E_abs (var, ty, e))
  in
  let k_type =
    mknoloc_bty @@ Bty_arrow (proc_sig.psig_ret_ty, mknoloc_bty @@ Bty_prim Pty_real)
  in
  let k_var = make_id "k" in
  mknoloc_exp @@ E_abs (k_var, k_type, e)
;;

let translate_proc_sig (proc_sigv : proc_sigv) =
  let tail =
    List.fold_right
      proc_sigv.psigv_arg_tys
      ~init:(Btyv_prim Pty_real)
      ~f:(fun (_, ty) tail -> Btyv_arrow (ty, tail))
  in
  let k_type = Btyv_arrow (proc_sigv.psigv_ret_ty, Btyv_prim Pty_real) in
  Btyv_arrow (k_type, tail)
;;

let elim_proc verbose (info_globals : IType.global_context) name proc =
  let%bind btyv =
    Typing.tycheck_proc verbose (ITyping.erase_label_global_context info_globals) proc
  in
  let k_var = make_id "k" in
  let k = mknoloc_exp @@ E_var k_var in
  let body' = equal_fmtnel_id_return k proc.proc_body in
  let psigv = Typing.erase_loc_proc_sig proc.proc_sig in
  let%bind locals = String.Map.of_alist_or_error psigv.psigv_arg_tys in
  let open ITyping in
  let fresh = Level.gen_new_level_var () in
  let k_type =
    IType.Lty
      ( Utyv_arrow
          (Lty (labelit_impl btyv, Leaf fresh), Lty (Utyv_prim Pty_real, Leaf fresh))
      , Leaf (LV_const L) )
  in
  let info_locals = String.Map.singleton k_var.txt k_type in
  let%bind e_body = elim_cmd verbose info_globals info_locals locals body' in
  let e = elim_proc_sig proc.proc_sig e_body in
  let sig' = translate_proc_sig @@ Typing.erase_loc_proc_sig @@ proc.proc_sig in
  let pure = Top_pure (name, to_base_ty sig', e) in
  if verbose then Format.printf "[elim_proc result]@.@[<hv>%a@]@\n" print_top_level pure;
  Ok pure
;;

let elim_prog verbose prog =
  let%bind { top_external_pure_sigvs; top_pure_sigvs; top_proc_sigvs } =
    ITyping.tycheck_prog verbose prog
  in
  let%bind non_info_top_proc_sigvs = Typing.collect_proc_sigs prog in
  let elim_globals = non_info_top_proc_sigvs |> String.Map.map ~f:translate_proc_sig in
  (* let globals =
     { Type.top_external_pure_sigvs = String.Map.empty
     ; top_proc_sigvs = String.Map.empty
     ; top_pure_sigvs = elim_globals
     } in *)
  let info_globals =
    { IType.top_external_pure_sigvs
    ; top_proc_sigvs
    ; top_pure_sigvs =
        map_merge (ITyping.labelit_context elim_globals) top_pure_sigvs
        (* TODO: think about orignizing non info globals and info globals *)
    }
  in
  if verbose then Format.printf "%a@\n" ITyping.print_global_context info_globals;
  let elimed =
    List.filter_map prog ~f:(fun top ->
      match top with
      | Top_proc (name, body) -> Some (elim_proc verbose info_globals name body)
      | Top_pure (name, bty, body) -> Some (Ok (Top_pure (name, bty, body)))
      | Top_external_pure (name, ty) -> Some (Ok (Top_external_pure (name, ty)))
      | _ -> None)
  in
  let succed, failed = List.partition_result elimed in
  if List.length failed > 0 then Error (List.hd_exn failed) else Ok succed
;;
