open Core
open Or_error.Let_syntax
open AbstractSyntaxTree
open Level
open LevelExpr
open Type
open Identifier
open InformationType
open Common

let inferred_is_L = function
  | Leaf (LV_const L) -> Ok true
  | Leaf (LV_const H) -> Ok false
  | Leaf (LV_var _) -> Ok true
  | _ -> Or_error.of_exn @@ failwith "level expression contains variable"
;;

let rec is_all_L_utyv = function
  | Utyv_prim _ -> Ok true
  | Utyv_var _ -> Ok true
  | Utyv_sum (lhs, rhs) ->
    let%bind b_lhs = is_all_L_ltyv lhs in
    let%bind b_rhs = is_all_L_ltyv rhs in
    Ok (b_lhs && b_rhs)
  | Utyv_prod (lhs, rhs) ->
    let%bind b_lhs = is_all_L_ltyv lhs in
    let%bind b_rhs = is_all_L_ltyv rhs in
    Ok (b_lhs && b_rhs)
  | Utyv_arrow (rator, rand) ->
    let%bind b_lhs = is_all_L_ltyv rator in
    let%bind b_rhs = is_all_L_ltyv rand in
    Ok (b_lhs && b_rhs)
  | Utyv_dist d -> is_all_L_ltyv d

and is_all_L_ltyv = function
  | Lty (utyv, l_expr) ->
    let%bind outter = inferred_is_L l_expr in
    let%bind inside = is_all_L_utyv utyv in
    Ok (outter && inside)
;;

let rec labelit_impl : base_tyv -> unlabeled_tyv = function
  | Btyv_prim p -> Utyv_prim p
  | Btyv_var name -> Utyv_var name
  | Btyv_sum (lhs, rhs) -> Utyv_sum (labelit lhs, labelit rhs)
  | Btyv_prod (lhs, rhs) -> Utyv_prod (labelit lhs, labelit rhs)
  | Btyv_arrow (rator, rand) -> Utyv_arrow (labelit rator, labelit rand)
  | Btyv_dist d -> Utyv_dist (labelit d)

and labelit bty : labeled_tyv = Lty (labelit_impl bty, Leaf (gen_new_unification_var ()))

let outterH bty = Lty (labelit_impl bty, Leaf (LV_const H))

let labelit_context (ctx : Type.context) : context =
  String.Map.map ctx ~f:(fun btyv -> labelit btyv)
;;

let rec erase_label_unlabeled_tyv : unlabeled_tyv -> base_tyv = function
  | Utyv_prim p -> Btyv_prim p
  | Utyv_var name -> Btyv_var name
  | Utyv_sum (lhs, rhs) ->
    Btyv_sum (erase_label_labeled_tyv lhs, erase_label_labeled_tyv rhs)
  | Utyv_prod (lhs, rhs) ->
    Btyv_prod (erase_label_labeled_tyv lhs, erase_label_labeled_tyv rhs)
  | Utyv_arrow (rator, rand) ->
    Btyv_arrow (erase_label_labeled_tyv rator, erase_label_labeled_tyv rand)
  | Utyv_dist d -> Btyv_dist (erase_label_labeled_tyv d)

and erase_label_labeled_tyv : labeled_tyv -> base_tyv = function
  | Lty (utyv, _) -> erase_label_unlabeled_tyv utyv
;;

let erase_label_context ctx = Map.map ctx ~f:erase_label_labeled_tyv

let rec fvars_to_uvars = function
  | Lty (utyv, l_expr) ->
    Lty (fvars_to_uvars_unlabeled utyv, fvars_to_uvars_level_expr l_expr)

and fvars_to_uvars_level_expr = function
  | Leaf (LV_const l) -> Leaf (LV_const l)
  | Leaf (LV_var _) -> Leaf (gen_new_unification_var ())
  | Leaf (LV_uvar _) -> failwith "impossible"
  | Join (lhs, rhs) -> Join (fvars_to_uvars_level_expr lhs, fvars_to_uvars_level_expr rhs)
  | _ -> failwith "[fvars_to_uvars_level_expr] TODO not implemented"

and fvars_to_uvars_unlabeled = function
  | Utyv_prim p -> Utyv_prim p
  | Utyv_var name -> Utyv_var name
  | Utyv_sum (lhs, rhs) -> Utyv_sum (fvars_to_uvars lhs, fvars_to_uvars rhs)
  | Utyv_prod (lhs, rhs) -> Utyv_prod (fvars_to_uvars lhs, fvars_to_uvars rhs)
  | Utyv_arrow (rator, rand) -> Utyv_arrow (fvars_to_uvars rator, fvars_to_uvars rand)
  | Utyv_dist d -> Utyv_dist (fvars_to_uvars d)
;;

let rec free_vars_in = function
  | Lty (utyv, l_expr) ->
    Set.union (free_vars_in_unlabeled utyv) (free_vars_in_level_expr l_expr)

and free_vars_in_level_expr = function
  | Leaf (LV_const _) -> Int.Set.empty
  | Leaf (LV_var i) -> Int.Set.singleton i
  | Leaf (LV_uvar _) -> Int.Set.empty
  | Join (lhs, rhs) ->
    Set.union (free_vars_in_level_expr lhs) (free_vars_in_level_expr rhs)
  | _ -> failwith "[free_vars_in_level_expr] TODO not implemented"

and free_vars_in_unlabeled = function
  | Utyv_prim _ -> Int.Set.empty
  | Utyv_var _ -> Int.Set.empty
  | Utyv_sum (lhs, rhs) -> Set.union (free_vars_in lhs) (free_vars_in rhs)
  | Utyv_prod (lhs, rhs) -> Set.union (free_vars_in lhs) (free_vars_in rhs)
  | Utyv_arrow (rator, rand) -> Set.union (free_vars_in rator) (free_vars_in rand)
  | Utyv_dist d -> free_vars_in d
;;

let subst_of_free_vars free_vars =
  Set.fold free_vars ~init:Int.Map.empty ~f:(fun s var ->
    Map.add_exn s ~key:var ~data:(Leaf (gen_new_unification_var ())))
;;

let rec visit_level_expr ~f = function
  | Lty (utyv, l_expr) -> Lty (subst_unlabeled ~f utyv, f l_expr)

and subst_unlabeled ~f = function
  | Utyv_prim p -> Utyv_prim p
  | Utyv_var name -> Utyv_var name
  | Utyv_sum (lhs, rhs) -> Utyv_sum (visit_level_expr ~f lhs, visit_level_expr ~f rhs)
  | Utyv_prod (lhs, rhs) -> Utyv_prod (visit_level_expr ~f lhs, visit_level_expr ~f rhs)
  | Utyv_arrow (rator, rand) ->
    Utyv_arrow (visit_level_expr ~f rator, visit_level_expr ~f rand)
  | Utyv_dist d -> Utyv_dist (visit_level_expr ~f d)
;;

let update_uvars s = visit_level_expr ~f:(update_uvars_only s)
let level_of (Lty (_, l)) = l

let outter_is_L = function
  | Lty (_, Leaf (LV_const L)) -> Ok true
  | Lty (_, Leaf (LV_const H)) -> Ok false
  | _ -> Or_error.of_exn @@ failwith "level expression contains variable"
;;

let erase_label_proc_sigv { psigv_arg_tys; psigv_ret_ty } =
  { Type.psigv_arg_tys = List.Assoc.map psigv_arg_tys ~f:erase_label_labeled_tyv
  ; psigv_ret_ty = erase_label_labeled_tyv psigv_ret_ty
  }
;;

let erase_label_global_context { top_external_pure_sigvs; top_pure_sigvs; top_proc_sigvs }
  =
  { Type.top_external_pure_sigvs = erase_label_context top_external_pure_sigvs
  ; top_pure_sigvs = erase_label_context top_pure_sigvs
  ; top_proc_sigvs = Map.map top_proc_sigvs ~f:erase_label_proc_sigv
  }
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

let rec is_subtype lhs rhs =
  match lhs, rhs with
  | Lty (lhs_utyv, _), Lty (rhs_utyv, _) ->
    (* TODO *)
    is_subtype_unlabeled lhs_utyv rhs_utyv

and is_subtype_unlabeled lhs rhs =
  match lhs, rhs with
  | Utyv_prim pty1, Utyv_prim pty2 -> is_prim_subtype pty1 pty2
  | Utyv_var name1, Utyv_var name2 -> String.(name1 = name2)
  | Utyv_sum (tyv11, tyv12), Utyv_sum (tyv21, tyv22) ->
    is_subtype tyv11 tyv21 && is_subtype tyv12 tyv22
  | Utyv_prod (tyv11, tyv12), Utyv_prod (tyv21, tyv22) ->
    is_subtype tyv11 tyv21 && is_subtype tyv12 tyv22
  | Utyv_arrow (tyv11, tyv12), Utyv_arrow (tyv21, tyv22) ->
    is_subtype tyv21 tyv11 && is_subtype tyv12 tyv22
  | Utyv_dist tyv1', Utyv_dist tyv2' -> equal_labeled_tyv tyv1' tyv2'
  | _ -> false
;;

let rec join_type ~loc lhs rhs : (labeled_tyv * level_constraint list) Or_error.t =
  match lhs, rhs with
  | Lty (lhs_utyv, lhs_level), Lty (rhs_utyv, rhs_level) ->
    let%bind utyv, constrs = join_type_unlabeled ~loc lhs_utyv rhs_utyv in
    let ufresh = gen_new_unification_var () in
    Ok
      ( Lty (utyv, Leaf ufresh)
      , Leq (lhs_level, Leaf ufresh) :: Leq (rhs_level, Leaf ufresh) :: constrs )

and join_type_unlabeled ~loc tyv1 tyv2
  : (unlabeled_tyv * level_constraint list) Or_error.t
  =
  match tyv1, tyv2 with
  | Utyv_prim pty1, Utyv_prim pty2 ->
    let%bind pty = Typing.join_prim ~loc pty1 pty2 in
    Ok (Utyv_prim pty, [])
  | Utyv_var name1, Utyv_var name2 when String.(name1 = name2) -> Ok (Utyv_var name1, [])
  | Utyv_sum (tyv11, tyv12), Utyv_sum (tyv21, tyv22) ->
    let%bind tyv1', constrs1 = join_type ~loc tyv11 tyv21 in
    let%bind tyv2', constrs2 = join_type ~loc tyv12 tyv22 in
    Ok (Utyv_prod (tyv1', tyv2'), constrs1 @ constrs2)
  | Utyv_prod (tyv11, tyv12), Utyv_prod (tyv21, tyv22) ->
    let%bind tyv1', constrs1 = join_type ~loc tyv11 tyv21 in
    let%bind tyv2', constrs2 = join_type ~loc tyv12 tyv22 in
    Ok (Utyv_prod (tyv1', tyv2'), constrs1 @ constrs2)
  | Utyv_arrow (tyv11, tyv12), Utyv_arrow (tyv21, tyv22) ->
    let%bind tyv1', constraints1 = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2', constraints2 = join_type ~loc tyv12 tyv22 in
    Ok (Utyv_arrow (tyv1', tyv2'), constraints1 @ constraints2)
  | Utyv_dist tyv1', Utyv_dist tyv2' ->
    if equal_labeled_tyv tyv1' tyv2'
    then Ok (Utyv_dist tyv1', [])
    else Or_error.of_exn (Type_error ("join error", loc))
  | _ -> Or_error.of_exn (Type_error ("join error", loc))

and meet_type ~loc lhs rhs : (labeled_tyv * level_constraint list) Or_error.t =
  match lhs, rhs with
  | Lty (lhs_utyv, lhs_level), Lty (rhs_utyv, rhs_level) ->
    let%bind utyv, constraints = meet_type_unlabeled ~loc lhs_utyv rhs_utyv in
    let ufresh = gen_new_unification_var () in
    Ok
      ( Lty (utyv, Leaf ufresh)
      , Leq (lhs_level, Leaf ufresh) :: Leq (rhs_level, Leaf ufresh) :: constraints )

and meet_type_unlabeled ~loc tyv1 tyv2 =
  match tyv1, tyv2 with
  | Utyv_prim pty1, Utyv_prim pty2 ->
    let%bind pty = Typing.meet_prim ~loc pty1 pty2 in
    Ok (Utyv_prim pty, [])
  | Utyv_var name1, Utyv_var name2 when String.(name1 = name2) -> Ok (Utyv_var name1, [])
  | Utyv_sum (tyv11, tyv12), Utyv_sum (tyv21, tyv22) ->
    let%bind tyv1', constraints1 = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2', constraints2 = meet_type ~loc tyv12 tyv22 in
    Ok (Utyv_prod (tyv1', tyv2'), constraints1 @ constraints2)
  | Utyv_prod (tyv11, tyv12), Utyv_prod (tyv21, tyv22) ->
    let%bind tyv1', constraints1 = meet_type ~loc tyv11 tyv21 in
    let%bind tyv2', constraints2 = meet_type ~loc tyv12 tyv22 in
    Ok (Utyv_prod (tyv1', tyv2'), constraints1 @ constraints2)
  | Utyv_arrow (tyv11, tyv12), Utyv_arrow (tyv21, tyv22) ->
    let%bind tyv1', constraints1 = join_type ~loc tyv11 tyv21 in
    let%bind tyv2', constraints2 = meet_type ~loc tyv12 tyv22 in
    Ok (Utyv_arrow (tyv1', tyv2'), constraints1 @ constraints2)
  | Utyv_dist tyv1', Utyv_dist tyv2' ->
    if equal_labeled_tyv tyv1' tyv2'
    then Ok (Utyv_dist tyv1', [])
    else Or_error.of_exn (Type_error ("meet error", loc))
  | _ -> Or_error.of_exn (Type_error ("meet error", loc))
;;

let is_prim_numeric = function
  | Pty_ureal | Pty_preal | Pty_real | Pty_fnat _ | Pty_nat -> true
  | _ -> false
;;

type labeled_proc_sigv =
  { lpsigv_lvars : string list
  ; lpsigv_arg_tys : (string * unlabeled_tyv) list
  ; lpsigv_ret_tyv : unlabeled_tyv
  }
[@@deriving show]

let rec print_unlabeled_tyv fmt = function
  | Utyv_prim pty -> print_prim_ty fmt pty
  | Utyv_var var -> Format.fprintf fmt "%s" var
  | Utyv_sum (lhs, rhs) ->
    Format.fprintf fmt "%a + %a" print_labeled_tyv lhs print_labeled_tyv rhs
  | Utyv_prod (lhs, rhs) ->
    Format.fprintf fmt "%a * %a" print_labeled_tyv lhs print_labeled_tyv rhs
  | Utyv_arrow (tyv1, tyv2) ->
    Format.fprintf fmt "%a -> %a" print_labeled_tyv tyv1 print_labeled_tyv tyv2
  | Utyv_dist tyv -> Format.fprintf fmt "%a dist" print_labeled_tyv tyv

and print_labeled_tyv fmt = function
  | Lty (utyv, l) ->
    Format.fprintf fmt "(%a){%a}" print_unlabeled_tyv utyv print_level_expr l
;;

let print_proc_arg_item fmt (var, bty) =
  Format.fprintf fmt "%s : %a" var print_labeled_tyv bty
;;

let print_ctx_item fmt (var, bty) =
  Format.fprintf fmt "\t%s\t: %a" var print_labeled_tyv bty
;;

let print_ctx fmt ctx =
  let kvs = Map.to_alist ctx in
  if List.is_empty kvs
  then Format.fprintf fmt "\t."
  else Format.pp_print_list ~pp_sep:Format.pp_force_newline print_ctx_item fmt kvs
;;

let print_proc_sigv fmt ({ psigv_arg_tys; psigv_ret_ty } : proc_sigv) =
  Format.fprintf
    fmt
    "(%a) -> %a"
    (print_list ~f:print_proc_arg_item)
    psigv_arg_tys
    print_labeled_tyv
    psigv_ret_ty
;;

let print_name_and_sigv fmt (name, sigv) =
  Format.fprintf fmt "\t%s\t: %a" name print_proc_sigv sigv
;;

let print_proc_sigvs fmt sigvs =
  let kvs = Map.to_alist sigvs in
  if List.is_empty kvs
  then Format.fprintf fmt "\t."
  else Format.pp_print_list ~pp_sep:Format.pp_force_newline print_name_and_sigv fmt kvs
;;

let print_global_context fmt { top_external_pure_sigvs; top_pure_sigvs; top_proc_sigvs } =
  Format.fprintf fmt "externals :\n%a\n" print_ctx top_external_pure_sigvs;
  Format.fprintf fmt "pures :\n%a\n" print_ctx top_pure_sigvs;
  Format.fprintf fmt "procs :\n%a\n\n" print_proc_sigvs top_proc_sigvs
;;

let label (utyv : unlabeled_tyv) ~(at : LevelExpr.t) : labeled_tyv = Lty (utyv, at)
let attach_L utyv = label utyv ~at:(Leaf (LV_const L))
let attach_fresh_uvar utyv = label utyv ~at:(Leaf (gen_new_unification_var ()))

let rec annotate_L (btyv : base_tyv) : labeled_tyv =
  match btyv with
  | Btyv_prim pty -> Lty (Utyv_prim pty, Leaf (LV_const L))
  | Btyv_var var -> Lty (Utyv_var var, Leaf (LV_const L))
  | Btyv_sum (lhs, rhs) ->
    Lty (Utyv_sum (annotate_L lhs, annotate_L rhs), Leaf (LV_const L))
  | Btyv_prod (lhs, rhs) ->
    Lty (Utyv_prod (annotate_L lhs, annotate_L rhs), Leaf (LV_const L))
  | Btyv_arrow (arg, ret) ->
    Lty (Utyv_arrow (annotate_L arg, annotate_L ret), Leaf (LV_const L))
  | Btyv_dist btyv -> Lty (Utyv_dist (annotate_L btyv), Leaf (LV_const L))
;;

let rec annotate_free (btyv : base_tyv) : labeled_tyv =
  match btyv with
  | Btyv_prim pty -> Lty (Utyv_prim pty, Leaf (gen_new_level_var ()))
  | Btyv_var var -> Lty (Utyv_var var, Leaf (gen_new_level_var ()))
  | Btyv_sum (lhs, rhs) ->
    Lty (Utyv_sum (annotate_free lhs, annotate_free rhs), Leaf (gen_new_level_var ()))
  | Btyv_prod (lhs, rhs) ->
    Lty (Utyv_prod (annotate_free lhs, annotate_free rhs), Leaf (gen_new_level_var ()))
  | Btyv_arrow (arg, ret) ->
    Lty (Utyv_arrow (annotate_free arg, annotate_free ret), Leaf (gen_new_level_var ()))
  | Btyv_dist btyv -> Lty (Utyv_dist (annotate_free btyv), Leaf (gen_new_level_var ()))
;;

let annotate_inside (btyv : base_tyv) : unlabeled_tyv =
  match btyv with
  | Btyv_prim pty -> Utyv_prim pty
  | Btyv_var var -> Utyv_var var
  | Btyv_sum (lhs, rhs) -> Utyv_sum (annotate_free lhs, annotate_free rhs)
  | Btyv_prod (lhs, rhs) -> Utyv_prod (annotate_free lhs, annotate_free rhs)
  | Btyv_arrow (arg, ret) -> Utyv_arrow (annotate_free arg, annotate_free ret)
  | Btyv_dist btyv -> Utyv_dist (annotate_free btyv)
;;

let annotate_L_context (ctx : Type.context) : context =
  String.Map.map ctx ~f:(fun btyv -> annotate_L btyv)
;;

let annotate_free_proc_sigv ({ psigv_arg_tys; psigv_ret_ty } : Type.proc_sigv) : proc_sigv
  =
  let fresh = gen_new_unification_var () in
  { psigv_arg_tys = List.Assoc.map psigv_arg_tys ~f:annotate_free
  ; psigv_ret_ty = Lty (annotate_inside psigv_ret_ty, Leaf fresh)
  }
;;

let collect_proc_sigs prog : proc_sigv String.Map.t Or_error.t =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_proc (proc_name, { proc_sig; _ }) ->
         Some (proc_name.txt, annotate_free_proc_sigv (Typing.erase_loc_proc_sig proc_sig))
       | _ -> None))
;;

let create_pure_itype (btyv : base_tyv) : labeled_tyv =
  match btyv with
  | Btyv_prim pty -> Lty (Utyv_prim pty, Leaf (gen_new_level_var ()))
  | Btyv_var var -> Lty (Utyv_var var, Leaf (gen_new_level_var ()))
  | Btyv_sum (lhs, rhs) ->
    Lty (Utyv_sum (annotate_free lhs, annotate_free rhs), Leaf (gen_new_level_var ()))
  | Btyv_prod (lhs, rhs) ->
    Lty (Utyv_prod (annotate_free lhs, annotate_free rhs), Leaf (gen_new_level_var ()))
  | Btyv_arrow (arg, ret) ->
    let rator = annotate_free arg in
    let rand = Lty (annotate_inside ret, Leaf (gen_new_unification_var ())) in
    Lty (Utyv_arrow (rator, rand), Leaf (LV_const L))
  | Btyv_dist btyv -> Lty (Utyv_dist (annotate_free btyv), Leaf (gen_new_level_var ()))
;;

let collect_pure_sigs prog : labeled_tyv String.Map.t Or_error.t =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_pure (name, bty, _) ->
         Some (name.txt, create_pure_itype (erase_loc_base_ty bty))
       | _ -> None))
;;

let set_to_union ctx =
  let tmp = Set.to_list ctx in
  normalize_level_expr @@ list_to_level_expr (List.map tmp ~f:(fun i -> Leaf (LV_var i)))
;;

let rec create_external_pure_itype_positive ctx (btyv : base_tyv) : labeled_tyv =
  match btyv with
  | Btyv_prim pty -> Lty (Utyv_prim pty, set_to_union ctx)
  | Btyv_var var -> Lty (Utyv_var var, set_to_union ctx)
  | Btyv_sum (lhs, rhs) ->
    Lty
      ( Utyv_sum
          ( create_external_pure_itype_positive ctx lhs
          , create_external_pure_itype_positive ctx rhs )
      , Leaf (LV_const L) )
  | Btyv_prod (lhs, rhs) ->
    Lty
      ( Utyv_prod
          ( create_external_pure_itype_positive ctx lhs
          , create_external_pure_itype_positive ctx rhs )
      , Leaf (LV_const L) )
  | Btyv_arrow (arg, ret) ->
    (match arg with
     | Btyv_prim _ | Btyv_var _ -> ()
     | _ -> failwith "no higher order types for external functions");
    let rator = annotate_free arg in
    let rator_levels = free_vars_in rator in
    let ctx' = Set.union ctx rator_levels in
    let rand = create_external_pure_itype_positive ctx' ret in
    Lty (Utyv_arrow (rator, rand), Leaf (LV_const L))
  | Btyv_dist btyv -> Lty (Utyv_dist (annotate_free btyv), Leaf (gen_new_level_var ()))
;;

let create_external_pure_itype (btyv : base_tyv) : labeled_tyv =
  create_external_pure_itype_positive Int.Set.empty btyv
;;

(* let create_external_pure_itype (btyv: base_tyv) : labeled_tyv =
   match btyv with
   | Btyv_prim pty -> Lty (Utyv_prim pty, Leaf (gen_new_level_var()))
   | Btyv_var var ->  Lty (Utyv_var var, Leaf (gen_new_level_var()))
   | Btyv_sum (lhs, rhs) -> Lty (Utyv_sum ((annotate_free lhs), (annotate_free rhs)), Leaf (gen_new_level_var()))
   | Btyv_prod (lhs, rhs) -> Lty (Utyv_prod ((annotate_free lhs), (annotate_free rhs)), Leaf (gen_new_level_var()))
   | Btyv_arrow (arg, ret) ->
   (
   match arg with
   | Btyv_prim _ | Btyv_var _ -> ()
   | _ -> failwith "no higher order types for external functions"
   );
   let rator = annotate_free arg in
   let rator_levels = free_vars_in rator in
   let tmp = Set.to_list rator_levels in
   let rator_level_join = list_to_level_expr (List.map tmp ~f:(fun i -> Leaf (LV_var i))) in
   let rand = Lty (annotate_inside ret, rator_level_join) in
   Lty (Utyv_arrow (rator, rand), Leaf (LV_const L))
   | Btyv_dist (btyv) -> Lty (Utyv_dist (annotate_free btyv), Leaf (gen_new_level_var())) *)

let collect_external_pures prog : labeled_tyv String.Map.t Or_error.t =
  String.Map.of_alist_or_error
    (List.filter_map prog ~f:(fun top ->
       match top with
       | Top_external_pure (var_name, ty) ->
         Some (var_name.txt, create_external_pure_itype (erase_loc_base_ty ty))
       | _ -> None))
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
  | _ -> Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))
;;

let tycheck_bop_unlabeled bop arg1 arg2 =
  match arg1, arg2 with
  | Utyv_prim pty1, Utyv_prim pty2 ->
    let%bind res = tycheck_bop_prim bop pty1 pty2 in
    Ok (Utyv_prim res)
  | _ -> Or_error.of_exn (Type_error ("mismatched operand types", bop.loc))
;;

let tycheck_bop bop lhs rhs : (labeled_tyv * level_constraint list) Or_error.t =
  match lhs, rhs with
  | Lty (lhs_utyv, lhs_level), Lty (rhs_utyv, rhs_level) ->
    let%bind utyv = tycheck_bop_unlabeled bop lhs_utyv rhs_utyv in
    let ufresh = gen_new_unification_var () in
    Ok
      ( label utyv ~at:(Leaf ufresh)
      , [ Leq (lhs_level, Leaf ufresh); Leq (rhs_level, Leaf ufresh) ] )
;;

(* Ok (Lty ((Utyv_prim Pty_nat), LV_const L)) *)
let rec tycheck_exp_impl
  verbose
  (globals : global_context)
  (locals : labeled_tyv String.Map.t)
  (exp : exp)
  : (labeled_tyv * level_constraint list) Or_error.t
  =
  match exp.exp_desc with
  | E_var var_name ->
    (match lookup_pures globals locals ~key:var_name.txt with
     | Some tyv -> Ok (tyv, [])
     | None ->
       if String.equal var_name.txt "LogSumExp"
       then Or_error.of_exn (Type_error (var_name.txt, exp.exp_loc))
       else
         Or_error.of_exn (Type_error ("undefined variable " ^ var_name.txt, exp.exp_loc)))
  | E_triv -> Ok (Lty (Utyv_prim Pty_unit, Leaf (LV_const L)), [])
  | E_bool _ -> Ok (Lty (Utyv_prim Pty_bool, Leaf (LV_const L)), [])
  | E_cond (exp0, exp1, exp2) ->
    let%bind Lty (tyv0, cond_level), constrs = tycheck_exp verbose globals locals exp0 in
    if is_subtype_unlabeled tyv0 (Utyv_prim Pty_bool)
    then (
      let%bind tyv1, constraints1 = tycheck_exp verbose globals locals exp1 in
      let%bind tyv2, constraints2 = tycheck_exp verbose globals locals exp2 in
      let%bind (Lty (_, join_level) as join_tyv), join_constraints =
        join_type ~loc:exp.exp_loc tyv1 tyv2
      in
      Ok
        ( join_tyv
        , (Leq (cond_level, join_level) :: constrs)
          @ constraints1
          @ constraints2
          @ join_constraints ))
    else Or_error.of_exn (Type_error ("non-boolean condition type", exp0.exp_loc))
  | E_real r ->
    if Float.(r >= 0. && r <= 1.)
    then Ok (Lty (Utyv_prim Pty_ureal, Leaf (LV_const L)), [])
    else if Float.(r >= 0.)
    then Ok (attach_L (Utyv_prim Pty_preal), [])
    else Ok (attach_L (Utyv_prim Pty_real), [])
  | E_nat n ->
    if n >= 0
    then Ok (attach_L (Utyv_prim (Pty_fnat (n + 1))), [])
    else Or_error.of_exn (Type_error ("negative integers", exp.exp_loc))
  | E_binop (bop, exp1, exp2) ->
    let%bind tyv1, constraints1 = tycheck_exp verbose globals locals exp1 in
    let%bind tyv2, constraints2 = tycheck_exp verbose globals locals exp2 in
    let%bind tyv, constraints = tycheck_bop bop tyv1 tyv2 in
    Ok (tyv, constraints @ constraints1 @ constraints2)
  | E_abs (arg_name, arg_type, body) ->
    let arg_tyv = annotate_free (erase_loc_base_ty arg_type) in
    let%bind ret_type, constraints =
      tycheck_exp verbose globals (Map.set locals ~key:arg_name.txt ~data:arg_tyv) body
    in
    Ok (attach_L (Utyv_arrow (arg_tyv, ret_type)), constraints)
  | E_app (exp1, exp2) ->
    let%bind rand_type, constraints2 = tycheck_exp verbose globals locals exp2 in
    (* let%bind rator, constraints1 = (match tycheck_exp verbose globals locals exp1 with
       | Error exn -> (
       match Error.to_exn exn with
       | Type_error (msg, loc) ->
       if (String.equal msg "LogSumExp") then
       match rand_type with
       | Lty(Utyv_arrow(a, Lty(Utyv_prim Pty_real,l_r)),l_k) ->
       Ok (Lty(Utyv_arrow(rand_type,  Lty(Utyv_prim Pty_real,l_r)),l_k), [])
       | _ -> Or_error.of_exn (Type_error ("Argument of of LogSumExp should be function type", exp.exp_loc))
       else
       Error exn
       | _ -> Error exn
       )
       | Ok (rator, constraints1) -> Ok (rator, constraints1)) in *)
    let%bind rator, constraints1 =
      match tycheck_exp verbose globals locals exp1 with
      | Error exn ->
        (match Error.to_exn exn with
         | Type_error (msg, _) ->
           if String.equal msg "LogSumExp"
           then (
             match rand_type with
             | Lty (Utyv_arrow (a, Lty (Utyv_prim Pty_real, l_r)), l_k) ->
               let ret_type = Lty (Utyv_prim Pty_real, l_r) in
               let ub_ret = Lty (Utyv_arrow (a, ret_type), Leaf (LV_const L)) in
               let lb_ub_ret = Lty (Utyv_arrow (a, ub_ret), Leaf (LV_const L)) in
               Ok (Lty (Utyv_arrow (rand_type, lb_ub_ret), l_k), [])
             (* Ok (Btyv_arrow(rand_type,  Btyv_arrow(a, Btyv_arrow (a, Btyv_prim Pty_real)))) *)
             | _ ->
               Or_error.of_exn
                 (Type_error
                    ("Argument of of LogSumExp should be function type", exp.exp_loc)))
           else Error exn
         | _ -> Error exn)
      | Ok rator -> Ok rator
    in
    let is_external =
      match exp1.exp_desc with
      | E_var name -> Map.mem globals.top_external_pure_sigvs name.txt
      | _ -> false
    in
    let () = assert ((not is_external) || List.is_empty constraints1) in
    (* let Lty (tyv1, level1) = if is_external then begin
       let fvars = free_vars_in rator in
       let s = subst_of_free_vars fvars in
       visit_level_expr ~f:(subst_vars_only s) rator
       end else rator in *)
    let fvars = free_vars_in rator in
    let s = subst_of_free_vars fvars in
    let (Lty (tyv1, level1)) = visit_level_expr ~f:(subst_vars_only s) rator in
    (match tyv1 with
     | Utyv_arrow (tyv11, tyv12) ->
       if is_subtype rand_type tyv11
       then (
         let (Lty (_, l11)) = tyv11 in
         let (Lty (_, l12)) = tyv12 in
         let (Lty (_, lrand)) = rand_type in
         Ok (tyv12, (Leq (lrand, l11) :: Leq (level1, l12) :: constraints1) @ constraints2))
       else
         Or_error.of_exn
           (Type_error ("[Information flow] mismatched argument types", exp2.exp_loc))
     | _ ->
       Or_error.of_exn
         (Type_error ("[Information flow] non-arrow function type", exp.exp_loc)))
  | E_let (exp1, var_name, exp2) ->
    let%bind tyv1, constraints1 = tycheck_exp verbose globals locals exp1 in
    let%bind locals' =
      Or_error.try_with (fun () -> Map.add_exn locals ~key:var_name.txt ~data:tyv1)
    in
    let%bind tyv2, constraints2 = tycheck_exp verbose globals locals' exp2 in
    Ok (tyv2, constraints1 @ constraints2)
  | E_dist dist ->
    let%bind tyv, constraints =
      tycheck_dist verbose ~loc:exp.exp_loc globals locals dist
    in
    Ok (attach_L (Utyv_dist tyv), constraints)
  | E_pair (exp1, exp2) ->
    let%bind tyv1, constraints1 = tycheck_exp verbose globals locals exp1 in
    let%bind tyv2, constraints2 = tycheck_exp verbose globals locals exp2 in
    let (Lty (_, lhs_level)) = tyv1 in
    let (Lty (_, rhs_level)) = tyv1 in
    let ufresh = gen_new_unification_var () in
    Ok
      ( label (Utyv_prod (tyv1, tyv2)) ~at:(Leaf ufresh)
      , (Leq (lhs_level, Leaf ufresh) :: Leq (rhs_level, Leaf ufresh) :: constraints1)
        @ constraints2 )
  | E_field (_, _) -> failwith "TODO: E_field not implemented"
  | E_inf -> Ok (Lty (Utyv_prim Pty_real, Leaf (LV_const L)), [])
  | E_ninf -> Ok (Lty (Utyv_prim Pty_real, Leaf (LV_const L)), [])
  | E_case (_, _, _, _, _) -> failwith "TODO: not implemented"
  | E_inl _ -> failwith "TODO: not implemented"
  | E_inr _ -> failwith "TODO: not implemented"
  | E_logPr (dist, obs) ->
    (* Format.printf "!!!%a\n" print_exp @@ mknoloc_exp (E_logPr (dist, obs)); *)
    let%bind obs_type, constraints_obs = tycheck_exp verbose globals locals obs in
    let%bind Lty (dist_type, dist_level), constraints_dist =
      tycheck_exp verbose globals locals dist
    in
    (match dist_type with
     | Utyv_dist bty ->
       (* Format.printf "!!!%a\n" print_labeled_tyv obs_type; *)
       (* Format.printf "%b\n" (is_subtype obs_type bty); *)
       if is_subtype obs_type bty
       then (
         let ufresh = gen_new_unification_var () in
         let (Lty (_, obs_level)) = obs_type in
         let (Lty (_, sample_level)) = bty in
         Ok
           ( label (Utyv_prim Pty_real) ~at:(Leaf ufresh)
           , (Leq (sample_level, Leaf ufresh)
              :: Leq (dist_level, Leaf ufresh)
              :: Leq (obs_level, Leaf ufresh)
              :: constraints_obs)
             @ constraints_dist ))
       else Or_error.of_exn (Type_error ("[logPr] mismatched types", dist.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | E_logML _ -> failwith "TODO: not implemented"

and tycheck_dist verbose ~loc globals locals dist
  : (labeled_tyv * level_constraint list) Or_error.t
  =
  let lift tars goal curs =
    let tcs = List.zip_exn tars curs in
    let%bind res =
      List.fold_result tcs ~init:None ~f:(fun acc (tar, cur) ->
        match cur with
        | Lty (Utyv_prim pty, _) when is_prim_subtype pty tar ->
          (* TODO *)
          (match acc with
           | None -> Ok (Some None)
           | Some None -> Ok (Some None)
           | _ -> Or_error.of_exn (Type_error ("mixed tensors and scalars", loc)))
        | _ -> Or_error.of_exn (Type_error ("mismatched parameter types", loc)))
    in
    let res = Option.value_exn res in
    match res with
    | None -> Ok (Utyv_prim goal)
    | _ -> failwith "tycheck_dist"
  in
  match dist with
  | D_ber exp ->
    let%bind args_tyv, constraints = tycheck_exp verbose globals locals exp in
    let%bind utyv = lift [ Pty_ureal ] Pty_bool [ args_tyv ] in
    let (Lty (_, l)) = args_tyv in
    Ok (label utyv ~at:l, constraints)
  | D_normal (exp1, exp2) ->
    let%bind tyv1, constraints1 = tycheck_exp verbose globals locals exp1 in
    let%bind tyv2, constraints2 = tycheck_exp verbose globals locals exp2 in
    let%bind utyv = lift [ Pty_real; Pty_preal ] Pty_real [ tyv1; tyv2 ] in
    let ufresh = gen_new_unification_var () in
    let (Lty (_, l1)) = tyv1 in
    let (Lty (_, l2)) = tyv2 in
    Ok
      ( label utyv ~at:(Leaf ufresh)
      , (Leq (l1, Leaf ufresh) :: Leq (l2, Leaf ufresh) :: constraints1) @ constraints2 )
  | D_beta (alpha, beta) ->
    let%bind tyv1, constraints1 = tycheck_exp verbose globals locals alpha in
    let%bind tyv2, constraints2 = tycheck_exp verbose globals locals beta in
    let%bind utyv = lift [ Pty_nat; Pty_nat ] Pty_ureal [ tyv1; tyv2 ] in
    let ufresh = gen_new_unification_var () in
    let (Lty (_, l1)) = tyv1 in
    let (Lty (_, l2)) = tyv2 in
    Ok
      ( label utyv ~at:(Leaf ufresh)
      , (Leq (l1, Leaf ufresh) :: Leq (l2, Leaf ufresh) :: constraints1) @ constraints2 )
  | _ -> failwith "[tycheck_dist] not implemented"

and tycheck_trm_impl verbose globals locals trm : (trm * level_constraint list) Or_error.t
  =
  match trm.trm_desc with
  | T_ret exp ->
    let%bind ty, constraints = tycheck_exp verbose globals locals exp in
    Ok (update_type_trm trm ty, constraints)
  | T_sample dist ->
    let%bind Lty (dist_type, dist_level), constraints =
      tycheck_exp verbose globals locals dist
    in
    (match dist_type with
     | Utyv_dist tyv ->
       let (Lty (sample_type, sample_level)) = tyv in
       let fresh = gen_new_unification_var () in
       let trm' = update_type_trm trm (Lty (sample_type, Leaf fresh)) in
       Ok
         ( trm'
         , Leq (sample_level, Leaf fresh) :: Leq (dist_level, Leaf fresh) :: constraints
         )
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | T_branch (e, tcmd, fcmd) ->
    let%bind Lty (e_type, e_level), constraints_cond =
      tycheck_exp verbose globals locals e
    in
    (match e_type with
     | Utyv_prim Pty_bool ->
       let%bind tcmd', constraints_t = tycheck_cmd verbose globals locals tcmd in
       let%bind fcmd', constraints_f = tycheck_cmd verbose globals locals fcmd in
       let (Lty (utyv_t, l_t)) = tcmd'.cmd_type |> Option.value_exn in
       let (Lty (utyv_f, l_f)) = fcmd'.cmd_type |> Option.value_exn in
       let%bind utyv, constraints_join =
         join_type_unlabeled ~loc:trm.trm_loc utyv_t utyv_f
       in
       let ufresh = gen_new_unification_var () in
       let trm_type = Some (label utyv ~at:(Leaf ufresh)) in
       let trm' =
         { trm_desc = T_branch (e, tcmd', fcmd'); trm_type; trm_loc = trm.trm_loc }
       in
       Ok
         ( trm'
         , (Leq (e_level, Leaf ufresh)
            :: Leq (l_f, Leaf ufresh)
            :: Leq (l_t, Leaf ufresh)
            :: constraints_join)
           @ constraints_cond
           @ constraints_t
           @ constraints_f )
     | _ -> Or_error.of_exn (Type_error ("non-boolean condition type", e.exp_loc)))
  | T_observe (dist, obs) ->
    let%bind obs_type, constraints_obs = tycheck_exp verbose globals locals obs in
    let%bind Lty (dist_type, dist_level), constraints_dist =
      tycheck_exp verbose globals locals dist
    in
    (match dist_type with
     | Utyv_dist bty ->
       if is_subtype obs_type bty
       then (
         let ufresh = gen_new_unification_var () in
         let (Lty (_, obs_level)) = obs_type in
         let (Lty (_, sample_level)) = bty in
         let trm' = update_type_trm trm @@ label (Utyv_prim Pty_unit) ~at:(Leaf ufresh) in
         Ok
           ( trm'
           , (Leq (sample_level, Leaf ufresh)
              :: Leq (dist_level, Leaf ufresh)
              :: Leq (obs_level, Leaf ufresh)
              :: constraints_obs)
             @ constraints_dist ))
       else Or_error.of_exn (Type_error ("[observe] mismatched types", obs.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("non-distribution types", dist.exp_loc)))
  | T_call (proc_name, args) ->
    (match Map.find globals.top_proc_sigvs proc_name.txt with
     | None ->
       Or_error.of_exn (Type_error ("unknown procedure " ^ proc_name.txt, proc_name.loc))
     | Some psigv ->
       if List.length psigv.psigv_arg_tys <> List.length args
       then Or_error.of_exn (Type_error ("mismatched arity", trm.trm_loc))
       else (
         let free_vars_list =
           List.map psigv.psigv_arg_tys ~f:(fun (_, tyv) -> free_vars_in tyv)
         in
         let free_vars = Int.Set.union_list free_vars_list in
         let s = subst_of_free_vars free_vars in
         (* let fvars = free_vars_in rator in
            let s = List.fold (Set.to_list fvars) ~init:Int.Map.empty ~f:(
            fun s i ->
            let fresh = gen_new_unification_var () in
            match fresh with
            | LV_uvar v -> Map.add_exn s ~key:i ~data: v
            | _ -> failwith "impossible"
            ) in
            subst s rator *)
         let%bind arg_tys, constraints =
           List.fold_result
             (List.rev args)
             ~init:([], [])
             ~f:(fun (acc_arg, acc_cons) arg ->
               let%bind arg_ty, constraints = tycheck_exp verbose globals locals arg in
               Ok (arg_ty :: acc_arg, constraints @ acc_cons))
         in
         if not
              (List.for_all2_exn arg_tys psigv.psigv_arg_tys ~f:(fun ty (_, ty') ->
                 is_subtype ty ty'))
         then
           Or_error.of_exn
             (Type_error ("[Information flow] mismatched argument types", trm.trm_loc))
         else (
           let param_tys =
             List.map psigv.psigv_arg_tys ~f:(fun (_, param) ->
               visit_level_expr ~f:(subst_vars_only s) param)
           in
           let new_constraints =
             List.map2_exn arg_tys param_tys ~f:(fun (Lty (_, l)) (Lty (_, l')) ->
               Leq (l, l'))
           in
           let ret_ty = visit_level_expr ~f:(subst_vars_only s) psigv.psigv_ret_ty in
           let trm' = update_type_trm trm ret_ty in
           Ok (trm', new_constraints @ constraints))))
  | T_factor exp ->
    let%bind exp_type, constraints = tycheck_exp verbose globals locals exp in
    (match exp_type with
     | Lty (Utyv_prim pty, l) ->
       if is_prim_subtype pty Pty_real
       then (
         let ret_ty = label (Utyv_prim Pty_unit) ~at:l in
         let trm' = update_type_trm trm ret_ty in
         Ok (trm', constraints))
       else Or_error.of_exn (Type_error ("mismatched types", exp.exp_loc))
     | _ -> Or_error.of_exn (Type_error ("mismatched types", exp.exp_loc)))
  | T_choose (lb, ub) ->
    let%bind lb_type, constraints_lb = tycheck_exp verbose globals locals lb in
    let%bind ub_type, constraints_ub = tycheck_exp verbose globals locals ub in
    (match lb_type, ub_type with
     | Lty (Utyv_prim pty_lb, l_lb), Lty (Utyv_prim pty_ub, l_ub) ->
       if is_prim_subtype pty_lb Pty_nat && is_prim_subtype pty_ub Pty_nat
       then (
         let fresh = gen_new_unification_var () in
         let ret_ty = label (Utyv_prim Pty_nat) ~at:(Leaf fresh) in
         let trm' = update_type_trm trm ret_ty in
         Ok
           ( trm'
           , (Leq (l_lb, Leaf fresh) :: Leq (l_ub, Leaf fresh) :: constraints_lb)
             @ constraints_ub ))
       else Or_error.of_exn (Type_error ("mismatched types", trm.trm_loc))
     | Lty (Utyv_prim _, _), _ ->
       Or_error.of_exn (Type_error ("mismatched types", ub.exp_loc))
     | _, _ -> Or_error.of_exn (Type_error ("mismatched types", lb.exp_loc)))
  | _ -> failwith "tycheck_trm_impl"

and tycheck_cmd_impl verbose globals locals cmd : (cmd * level_constraint list) Or_error.t
  =
  match cmd.cmd_desc with
  | M_trm trm ->
    let%bind trm', constraints = tycheck_trm verbose globals locals trm in
    let cmd' =
      { cmd_desc = M_trm trm'; cmd_type = trm'.trm_type; cmd_loc = cmd.cmd_loc }
    in
    Ok (cmd', constraints)
  | M_seq (t, m) ->
    let%bind t', t_constraints = tycheck_trm verbose globals locals t in
    let%bind m', m_constraints = tycheck_cmd verbose globals locals m in
    let (Lty (_, t_level)) = t'.trm_type |> Option.value_exn in
    let (Lty (m_utyv, m_level)) = m'.cmd_type |> Option.value_exn in
    let ufresh = gen_new_unification_var () in
    let cmd_type = Some (label m_utyv ~at:(Leaf ufresh)) in
    let cmd' = { cmd_desc = M_seq (t', m'); cmd_type; cmd_loc = cmd.cmd_loc } in
    Ok
      ( cmd'
      , (Leq (t_level, Leaf ufresh) :: Leq (m_level, Leaf ufresh) :: t_constraints)
        @ m_constraints )
  | M_bnd (var, t, m) ->
    let%bind t', t_constraints = tycheck_trm verbose globals locals t in
    let (Lty (t_utyv, t_level)) = t'.trm_type |> Option.value_exn in
    let var_type = attach_L t_utyv in
    let locals' = Map.add_exn locals ~key:var.txt ~data:var_type in
    let%bind m', m_constraints = tycheck_cmd verbose globals locals' m in
    let (Lty (m_utyv, m_level)) = m'.cmd_type |> Option.value_exn in
    let ufresh = gen_new_unification_var () in
    let cmd_type = Some (label m_utyv ~at:(Leaf ufresh)) in
    let cmd' = { cmd_desc = M_bnd (var, t', m'); cmd_type; cmd_loc = cmd.cmd_loc } in
    Ok
      ( cmd'
      , (Leq (t_level, Leaf ufresh) :: Leq (m_level, Leaf ufresh) :: t_constraints)
        @ m_constraints )

and tycheck_cmd verbose globals locals cmd : (cmd * level_constraint list) Or_error.t =
  let%bind cmd', constraints = tycheck_cmd_impl verbose globals locals cmd in
  if verbose
  then
    Format.printf
      "[tycheck_cmd] %a |- %a : %a\n[tycheck_cmd] %a\n\n"
      print_ctx
      locals
      print_cmd
      cmd
      print_labeled_tyv
      (Option.value_exn cmd'.cmd_type)
      (print_list ~f:print_level_constraint)
      constraints;
  Ok (cmd', constraints)

and tycheck_trm verbose globals locals trm : (trm * level_constraint list) Or_error.t =
  (* let () = print_endline "tycheck_trm" in  *)
  (* let () = print_endline (show_trm trm) in *)
  let%bind trm', constraints = tycheck_trm_impl verbose globals locals trm in
  let trm_ty = trm'.trm_type |> Option.value_exn in
  if verbose
  then
    Format.printf
      "[tycheck_trm] %a |- %a : %a\n[tycheck_trm] %a\n\n"
      print_ctx
      locals
      print_trm
      trm
      print_labeled_tyv
      trm_ty
      (print_list ~f:print_level_constraint)
      constraints;
  Ok (trm', constraints)

and tycheck_exp verbose globals locals exp
  : (labeled_tyv * level_constraint list) Or_error.t
  =
  let%bind bty, constraints = tycheck_exp_impl verbose globals locals exp in
  if verbose
  then
    Format.printf
      "[tycheck_exp type]\n%a\n\t|-\n\t%a\t: %a\n[tycheck_exp constrain] %a\n\n"
      print_ctx
      locals
      print_exp
      exp
      print_labeled_tyv
      bty
      (print_list ~f:print_level_constraint)
      constraints;
  Ok (bty, constraints)
;;

let tycheck_proc verbose globals proc name =
  (* let psigv = erase_loc_proc_sig proc.proc_sig in *)
  (* let%bind locals = String.Map.of_alist_or_error (List.concat [ psigv.psigv_arg_tys ]) in *)
  let psigv = Map.find_exn globals.top_proc_sigvs name in
  let%bind locals = String.Map.of_alist_or_error psigv.psigv_arg_tys in
  let%bind cmd', constraints = tycheck_cmd verbose globals locals proc.proc_body in
  let (Lty (bty, bty_level)) = cmd'.cmd_type |> Option.value_exn in
  let (Lty (ret, ret_level)) = psigv.psigv_ret_ty in
  if not (is_subtype_unlabeled bty ret)
  then Or_error.of_exn (Type_error ("mismatched signature types", proc.proc_loc))
  else (
    let%bind s = Solver.solve verbose (Leq (bty_level, ret_level) :: constraints) in
    let infered_ret_ty = visit_level_expr ~f:(subst_uvars_only s) psigv.psigv_ret_ty in
    let infered =
      { psigv_arg_tys = psigv.psigv_arg_tys; psigv_ret_ty = infered_ret_ty }
    in
    Ok infered)
;;

let tycheck_pure verbose globals btyv body =
  let%bind pure_sig, constraints = tycheck_exp verbose globals String.Map.empty body in
  if not (is_subtype pure_sig btyv) (* TODO *)
  then Or_error.of_exn (Type_error ("mismatched signature types", body.exp_loc))
  else (
    let%bind s = Solver.solve verbose constraints in
    let infered = visit_level_expr ~f:(subst_uvars_only s) btyv in
    Ok infered)
;;

let type_check verbose globals locals code ~f =
  let%bind ty, constraints = f verbose globals locals code in
  let%bind s = Solver.solve verbose constraints in
  let infered =
    visit_level_expr ~f:(Fn.compose normalize_level_expr (subst_uvars_only s)) ty
  in
  Ok infered
;;

let type_check_trm verbose globals locals trm =
  let%bind trm', constraints = tycheck_trm verbose globals locals trm in
  let%bind s = Solver.solve verbose constraints in
  let ty = trm'.trm_type |> Option.value_exn in
  let infered =
    visit_level_expr ~f:(Fn.compose normalize_level_expr (subst_uvars_only s)) ty
  in
  Ok infered
;;

let tycheck_prog verbose prog =
  reset_level_variable ();
  let%bind top_external_pure_sigvs = collect_external_pures prog in
  let%bind top_proc_sigvs = collect_proc_sigs prog in
  let%bind top_pure_sigvs = collect_pure_sigs prog in
  if verbose
  then (
    Format.printf "\nexternals : %a\n" print_ctx top_external_pure_sigvs;
    Format.printf "\npures : %a\n" print_ctx top_pure_sigvs;
    Format.printf "procs : %a\n" print_proc_sigvs top_proc_sigvs);
  let global : global_context =
    { top_external_pure_sigvs; top_proc_sigvs; top_pure_sigvs }
  in
  let tycheck_results =
    List.filter_map prog ~f:(fun top ->
      match top with
      | Top_proc (proc_name, proc) ->
        Some
          (let%bind result = tycheck_proc verbose global proc proc_name.txt in
           Ok (proc_name.txt, result))
      | _ -> None)
  in
  let succed, failed = List.partition_result tycheck_results in
  if List.length failed > 0
  then Error (List.hd_exn failed)
  else (
    let infered_proc_sigvs = String.Map.of_alist_exn succed in
    let infered_global =
      { top_external_pure_sigvs; top_proc_sigvs = infered_proc_sigvs; top_pure_sigvs }
    in
    Ok infered_global)
;;

let rec subst_level_cmd s { cmd_loc; cmd_type; cmd_desc } =
  let cmd_type =
    cmd_type
    |> Option.value_exn
    |> visit_level_expr ~f:(Fn.compose normalize_level_expr (subst_uvars_only s))
    |> Option.return
  in
  match cmd_desc with
  | M_trm t ->
    let t' = subst_level_trm s t in
    { cmd_desc = M_trm t'; cmd_type; cmd_loc }
  | M_seq (t, m) ->
    let t' = subst_level_trm s t in
    let m' = subst_level_cmd s m in
    { cmd_desc = M_seq (t', m'); cmd_type; cmd_loc }
  | M_bnd (id, t, m) ->
    let t' = subst_level_trm s t in
    let m' = subst_level_cmd s m in
    { cmd_desc = M_bnd (id, t', m'); cmd_type; cmd_loc }

and subst_level_trm s { trm_loc; trm_desc; trm_type } =
  let trm_type' =
    trm_type
    |> Option.value_exn
    |> visit_level_expr ~f:(Fn.compose normalize_level_expr (subst_uvars_only s))
    |> Option.return
  in
  { trm_loc; trm_desc; trm_type = trm_type' }
;;

let rec print_typed_cmd fmt cmd =
  match cmd.cmd_desc with
  | M_trm trm ->
    Format.fprintf
      fmt
      "@[@[%a@] : @[%a@]"
      print_trm
      trm
      print_labeled_tyv
      (Option.value_exn trm.trm_type)
  | M_bnd (var, trm, cmd) ->
    Format.fprintf
      fmt
      "@[@[%s = @[%a@] : @[%a@]];@.@[%a@]@]"
      var.txt
      print_trm
      trm
      print_labeled_tyv
      (Option.value_exn trm.trm_type)
      print_typed_cmd
      cmd
  | M_seq (trm, cmd) ->
    Format.fprintf
      fmt
      "@[@[%a@] : @[%a@];@.@[%a@]@]"
      print_trm
      trm
      print_labeled_tyv
      (Option.value_exn trm.trm_type)
      print_typed_cmd
      cmd
;;
