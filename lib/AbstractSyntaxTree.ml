open Core
open Identifier
open Type
open Common

type binop =
  | Bop_add
  | Bop_sub
  | Bop_mul
  | Bop_div
  | Bop_eq
  | Bop_ne
  | Bop_lt
  | Bop_le
  | Bop_gt
  | Bop_ge
  | Bop_and
  | Bop_or
[@@deriving show, equal, compare, hash]

let print_binop fmt bop =
  match bop with
  | Bop_add -> Format.fprintf fmt "%s" "+"
  | Bop_sub -> Format.fprintf fmt "%s" "-"
  | Bop_mul -> Format.fprintf fmt "%s" "*"
  | Bop_div -> Format.fprintf fmt "%s" "/"
  | Bop_eq -> Format.fprintf fmt "%s" "="
  | Bop_ne -> Format.fprintf fmt "%s" "<>"
  | Bop_lt -> Format.fprintf fmt "%s" "<"
  | Bop_le -> Format.fprintf fmt "%s" "<="
  | Bop_gt -> Format.fprintf fmt "%s" ">"
  | Bop_ge -> Format.fprintf fmt "%s" ">="
  | Bop_and -> Format.fprintf fmt "%s" "and"
  | Bop_or -> Format.fprintf fmt "%s" "or"
;;

type 'a dist =
  | D_ber of 'a
  | D_unif
  | D_beta of 'a * 'a
  | D_gamma of 'a * 'a
  | D_normal of 'a * 'a
  | D_cat of 'a list
  | D_bin of int * 'a
  | D_geo of 'a
  | D_pois of 'a
[@@deriving show, equal, compare, hash]

type exp =
  { exp_desc : exp_desc
  ; exp_loc : Location.t
  }

and exp_desc =
  | E_var of variable_id
  | E_triv
  | E_bool of bool
  | E_cond of exp * exp * exp
  | E_case of exp * variable_id * exp * variable_id * exp
  | E_real of float
  | E_nat of int
  | E_inf
  | E_ninf
  | E_binop of binop loc * exp * exp
  | E_abs of variable_id * base_ty * exp
  | E_app of exp * exp
  | E_inl of exp
  | E_inr of exp
  | E_let of exp * variable_id * exp
  | E_dist of exp dist
  | E_logPr of exp * exp
  | E_logML of cmd
  | E_pair of exp * exp
  | E_field of exp * int
[@@deriving show, equal, compare, hash]

and trm =
  { trm_desc : trm_desc
  ; trm_loc : Location.t [@printer Location.print_loc]
  ; trm_type : InformationType.labeled_tyv option
  }

and trm_desc =
  | T_ret of exp
  | T_sample of exp
  | T_factor of exp
  | T_observe of exp * exp
  | T_branch of exp * cmd * cmd
  | T_case of exp * variable_id * cmd * variable_id * cmd
  | T_call of def_id * exp list
  | T_choose of exp * exp

and cmd =
  { cmd_desc : cmd_desc
  ; cmd_loc : Location.t [@printer Location.print_loc]
  ; cmd_type : InformationType.labeled_tyv option
  }

and cmd_desc =
  | M_trm of trm
  | M_seq of trm * cmd
  | M_bnd of variable_id * trm * cmd
[@@deriving show]

let mknoloc_exp exp_desc = { exp_desc; exp_loc = Location.none }
let mknoloc_trm trm_desc = { trm_desc; trm_loc = Location.none; trm_type = None }
let mknoloc_cmd cmd_desc = { cmd_desc; cmd_loc = Location.none; cmd_type = None }

let update_type_trm { trm_desc; trm_loc; _ } ty =
  { trm_desc; trm_loc; trm_type = Some ty }
;;

let update_desc_cmd { cmd_loc; cmd_type; _ } cmd_desc = { cmd_desc; cmd_loc; cmd_type }
let print_arg fmt (var, bty) = Format.fprintf fmt "%s : %a" var print_base_tyv bty

let erase_loc_arg_list l =
  List.map l ~f:(fun (name, basety) -> name.txt, erase_loc_base_ty basety)
;;

let rec print_exp fmt exp =
  match exp.exp_desc with
  | E_var var_name -> Format.fprintf fmt "%s" var_name.txt
  | E_triv -> Format.fprintf fmt "%s" "unit"
  | E_bool b -> Format.fprintf fmt "%b" b
  | E_cond (cond, tbranch, fbranch) ->
    Format.fprintf
      fmt
      "@[<hv>if %a then@;<1 4>@[%a@]@;else@;<1 4>@[%a@]@;end@]"
      (* "@[<v 2>if (%a) then@\n%a@ else@\n%a end@]" *)
      (* "if %a then\n@[<hov 2>%a@] else @[<hov 2>%a@] end" *)
      print_exp
      cond
      print_exp
      tbranch
      print_exp
      fbranch
  | E_real r -> Format.fprintf fmt "%f" r
  | E_nat n -> Format.fprintf fmt "%d" n
  | E_binop (bop, lhs, rhs) ->
    Format.fprintf
      fmt
      "@[<hv>@[%a@] @[%a@]@;@[%a@]@]"
      print_exp
      lhs
      print_binop
      bop.txt
      print_exp
      rhs
  | E_dist d -> print_dist fmt d
  | E_abs (arg_name, arg_type, body) ->
    Format.fprintf
      fmt
      "fun (%s : @[%a@]) =>@;<1 4>@[%a@]"
      arg_name.txt
      print_base_tyv
      (erase_loc_base_ty arg_type)
      print_exp
      body
  | E_app (rator, rand) -> Format.fprintf fmt "(%a %a)" print_exp rator print_exp rand
  | E_field (exp0, field) -> Format.fprintf fmt "%a[%d]" print_exp exp0 field
  | E_let ({ exp_desc = E_abs (arg_name, arg_type, body); exp_loc }, v, e) ->
    let eabs = { exp_desc = E_abs (arg_name, arg_type, body); exp_loc } in
    Format.fprintf
      fmt
      "@[<hv>let %s = %a@;in@;@[%a@]@;end@]"
      v.txt
      print_exp
      eabs
      print_exp
      e
  | E_let (ev, v, e) ->
    Format.fprintf
      fmt
      "@[<v 0>let %s = %a in@;@[%a@]@;end@]"
      v.txt
      print_exp
      ev
      print_exp
      e
  | E_pair (exp1, exp2) -> Format.fprintf fmt "(%a, %a)" print_exp exp1 print_exp exp2
  | E_inf -> Format.fprintf fmt "inf"
  | E_ninf -> Format.fprintf fmt "-inf"
  | E_case (_, _, _, _, _) -> failwith "TODO: not implemented"
  | E_inl _ -> failwith "TODO: not implemented"
  | E_inr _ -> failwith "TODO: not implemented"
  | E_logPr (dist, v) ->
    Format.fprintf fmt "logPr %a at %a end" print_exp dist print_exp v
  | E_logML m -> Format.fprintf fmt "@[<hv>logML(@;<0 4>@[%a@]@;)@]" print_cmd m

and print_dist fmt d =
  match d with
  | D_ber e -> Format.fprintf fmt "BERN(%a)" print_exp e
  | D_normal (e1, e2) -> Format.fprintf fmt "NORMAL(%a, %a)" print_exp e1 print_exp e2
  | D_unif -> Format.fprintf fmt "UNIF"
  | D_beta (e1, e2) -> Format.fprintf fmt "BETA(%a, %a)" print_exp e1 print_exp e2
  | D_gamma (e1, e2) -> Format.fprintf fmt "GAMMA(%a, %a)" print_exp e1 print_exp e2
  | D_cat args ->
    Format.fprintf
      fmt
      "CAT(%a)"
      (print_list ~f:(fun fmt e -> Format.fprintf fmt "%a" print_exp e))
      args
  | D_bin (n, e) -> Format.fprintf fmt "BIN(%d; %a)" n print_exp e
  | D_geo e -> Format.fprintf fmt "GEO(%a)" print_exp e
  | D_pois e -> Format.fprintf fmt "POIS(%a)" print_exp e

and print_trm fmt trm =
  match trm.trm_desc with
  | T_ret exp -> Format.fprintf fmt "return %a" print_exp exp
  | T_sample e -> Format.fprintf fmt "sample(%a)" print_exp e
  | T_branch (cond, tbranch, fbranch) ->
    Format.fprintf
      fmt
      "if {%a} then %a else %a"
      print_exp
      cond
      print_cmd
      tbranch
      print_cmd
      fbranch
  | T_factor exp -> Format.fprintf fmt "@[factor(@;<0 4>@[%a@]@;)@]" print_exp exp
  | T_observe (dist, obs) ->
    Format.fprintf fmt "observe %a from %a" print_exp obs print_exp dist
  | T_case _ -> failwith "not implemented "
  | T_call (def_id, args) ->
    Format.fprintf fmt "%s(%a)" def_id.txt (print_list ~f:print_exp) args
  | T_choose (lb, ub) -> Format.fprintf fmt "choose(%a,%a)" print_exp lb print_exp ub

and print_cmd fmt cmd =
  match cmd.cmd_desc with
  | M_trm trm -> print_trm fmt trm
  | M_bnd (var, trm, cmd) ->
    Format.fprintf fmt "@[<v 0>%s = %a;@;@[%a@]@]" var.txt print_trm trm print_cmd cmd
  | M_seq (trm, cmd) ->
    Format.fprintf fmt "@[<v 0>%a;@;@[%a@]@]" print_trm trm print_cmd cmd
;;

(* let erase_loc_proc_sig psig =
     { psigv_arg_tys =
         List.map psig.psig_arg_tys ~f:(fun (var_name, ty) ->
             var_name.txt, erase_loc_base_ty ty)
     ; psigv_ret_ty = erase_loc_base_ty psig.psig_ret_ty
     }
   ;; *)

type proc =
  { proc_sig : proc_sig
  ; proc_body : cmd
  ; proc_loc : Location.t [@printer Location.print_loc]
  }
[@@deriving show]

type top_level =
  | Top_type of type_id * base_ty
  | Top_pure of def_id * base_ty * exp
  | Top_proc of def_id * proc
  | Top_external_type of type_id
  | Top_external_pure of variable_id * base_ty
[@@deriving show]

let print_top_level fmt = function
  | Top_pure (id, ty, body) ->
    Format.fprintf
      fmt
      "@[def %s : @[%a@] =@.    @[%a@]@]@\n"
      id.txt
      print_base_tyv
      (erase_loc_base_ty ty)
      print_exp
      body
  | Top_external_pure (id, ty) ->
    Format.fprintf
      fmt
      "@[external def %s : @[%a@]@]@\n"
      id.txt
      print_base_tyv
      (erase_loc_base_ty ty)
  | _ -> failwith "[TODO] print_top_level"
;;

type prog = top_level list [@@deriving show]

let rec apperas_exp exp name =
  (* Format.printf "[apperas_exp]@[%a@]@." print_exp exp; *)
  match exp.exp_desc with
  | E_var id -> String.equal id.txt name
  | E_real _ -> false
  | E_nat _ -> false
  | E_triv -> false
  | E_bool _ -> false
  | E_abs (id, _, e) -> if String.equal id.txt name then false else apperas_exp e name
  | E_app (e1, e2) -> apperas_exp e1 name || apperas_exp e2 name
  | E_binop (_, lhs, rhs) -> apperas_exp lhs name || apperas_exp rhs name
  | E_logPr (dist, v) -> apperas_exp dist name || apperas_exp v name
  | E_dist (D_ber p) -> apperas_exp p name
  | E_let (e1, id, e2) ->
    if String.equal id.txt name
    then apperas_exp e1 name
    else apperas_exp e1 name || apperas_exp e2 name
  | E_cond (econd, etrue, efalse) ->
    apperas_exp econd name || apperas_exp etrue name || apperas_exp efalse name
  | E_logML m -> apperas_cmd m name
  | E_dist (D_beta (a, b)) -> apperas_exp a name || apperas_exp b name
  | E_dist (D_normal (loc, std)) -> apperas_exp loc name || apperas_exp std name
  | _ ->
    Format.printf "%a@\n" print_exp exp;
    failwith "[apperas] exp TODO"

and apperas_trm trm name =
  match trm.trm_desc with
  | T_branch (e, cmd_t, cmd_f) ->
    apperas_exp e name || apperas_cmd cmd_t name || apperas_cmd cmd_f name
  | T_call (_, args) -> args |> List.exists ~f:(fun arg -> apperas_exp arg name)
  | T_factor e -> apperas_exp e name
  | T_ret e -> apperas_exp e name
  | T_sample e -> apperas_exp e name
  | _ ->
    Format.printf "%a@\n" print_trm trm;
    failwith "[apperas] trm TODO"

and apperas_cmd cmd name =
  match cmd.cmd_desc with
  | M_trm trm -> apperas_trm trm name
  | M_seq (trm, cmd) -> apperas_trm trm name || apperas_cmd cmd name
  | M_bnd (id, trm, cmd) ->
    if String.equal id.txt name
    then apperas_trm trm name
    else apperas_trm trm name || apperas_cmd cmd name
;;

let dump_ast_prog prog = Format.asprintf "%a@." pp_prog prog

let print_prog fmt prog =
  Format.fprintf fmt "@[%a@]" (Format.pp_print_list print_top_level) prog
;;

let rec subst_exp s exp =
  match exp.exp_desc with
  | E_var id -> Map.find s id.txt |> Option.value ~default:exp
  | E_abs (id, ty, body) ->
    let s' = Map.update s id.txt ~f:(fun _ -> mknoloc_exp @@ E_var id) in
    mknoloc_exp @@ E_abs (id, ty, subst_exp s' body)
  | E_let (e, id, body) -> mknoloc_exp @@ E_let (subst_exp s e, id, subst_exp s body)
  | E_cond (econd, etrue, efalse) ->
    mknoloc_exp @@ E_cond (subst_exp s econd, subst_exp s etrue, subst_exp s efalse)
  | E_binop (bop, lhs, rhs) ->
    mknoloc_exp @@ E_binop (bop, subst_exp s lhs, subst_exp s rhs)
  | E_app (lhs, rhs) -> mknoloc_exp @@ E_app (subst_exp s lhs, subst_exp s rhs)
  | _ -> exp
;;
